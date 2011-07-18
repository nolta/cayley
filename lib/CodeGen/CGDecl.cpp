//===--- CGDecl.cpp - Emit LLVM Code for declarations ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit Decl nodes as LLVM code.
//
//===----------------------------------------------------------------------===//

#include "CGDebugInfo.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CharUnits.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Intrinsics.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Type.h"
using namespace clang;
using namespace CodeGen;


void CodeGenFunction::EmitDecl(const Decl &D) {
  switch (D.getKind()) {
  case Decl::TranslationUnit:
  case Decl::Namespace:
  case Decl::UnresolvedUsingTypename:
  case Decl::ClassTemplateSpecialization:
  case Decl::ClassTemplatePartialSpecialization:
  case Decl::TemplateTypeParm:
  case Decl::UnresolvedUsingValue:
  case Decl::NonTypeTemplateParm:
  case Decl::CXXMethod:
  case Decl::CXXConstructor:
  case Decl::CXXDestructor:
  case Decl::CXXConversion:
  case Decl::Field:
  case Decl::IndirectField:
  case Decl::ObjCIvar:
  case Decl::ObjCAtDefsField:      
  case Decl::ParmVar:
  case Decl::ImplicitParam:
  case Decl::ClassTemplate:
  case Decl::FunctionTemplate:
  case Decl::TypeAliasTemplate:
  case Decl::TemplateTemplateParm:
  case Decl::ObjCMethod:
  case Decl::ObjCCategory:
  case Decl::ObjCProtocol:
  case Decl::ObjCInterface:
  case Decl::ObjCCategoryImpl:
  case Decl::ObjCImplementation:
  case Decl::ObjCProperty:
  case Decl::ObjCCompatibleAlias:
  case Decl::AccessSpec:
  case Decl::LinkageSpec:
  case Decl::ObjCPropertyImpl:
  case Decl::ObjCClass:
  case Decl::ObjCForwardProtocol:
  case Decl::FileScopeAsm:
  case Decl::Friend:
  case Decl::FriendTemplate:
  case Decl::Block:
    assert(0 && "Declaration should not be in declstmts!");
  case Decl::Function:  // void X();
  case Decl::Record:    // struct/union/class X;
  case Decl::Enum:      // enum X;
  case Decl::EnumConstant: // enum ? { X = ? }
  case Decl::CXXRecord: // struct/union/class X; [C++]
  case Decl::Using:          // using X; [C++]
  case Decl::UsingShadow:
  case Decl::UsingDirective: // using namespace X; [C++]
  case Decl::NamespaceAlias:
  case Decl::StaticAssert: // static_assert(X, ""); [C++0x]
  case Decl::Label:        // __label__ x;
    // None of these decls require codegen support.
    return;

  case Decl::Var: {
    const VarDecl &VD = cast<VarDecl>(D);
    assert(VD.isLocalVarDecl() &&
           "Should not see file-scope variables inside a function!");
    return EmitVarDecl(VD);
  }

  case Decl::Typedef:      // typedef int X;
  case Decl::TypeAlias: {  // using X = int; [C++0x]
    const TypedefNameDecl &TD = cast<TypedefNameDecl>(D);
    QualType Ty = TD.getUnderlyingType();

    if (Ty->isVariablyModifiedType())
      EmitVariablyModifiedType(Ty);
  }
  }
}

/// EmitVarDecl - This method handles emission of any variable declaration
/// inside a function, including static vars etc.
void CodeGenFunction::EmitVarDecl(const VarDecl &D) {
  switch (D.getStorageClass()) {
  case SC_None:
  case SC_Auto:
  case SC_Register:
    return EmitAutoVarDecl(D);
  case SC_Static: {
    llvm::GlobalValue::LinkageTypes Linkage = 
      llvm::GlobalValue::InternalLinkage;

    // If the function definition has some sort of weak linkage, its
    // static variables should also be weak so that they get properly
    // uniqued.  We can't do this in C, though, because there's no
    // standard way to agree on which variables are the same (i.e.
    // there's no mangling).
    if (getContext().getLangOptions().CPlusPlus)
      if (llvm::GlobalValue::isWeakForLinker(CurFn->getLinkage()))
        Linkage = CurFn->getLinkage();
    
    return EmitStaticVarDecl(D, Linkage);
  }
  case SC_Extern:
  case SC_PrivateExtern:
    // Don't emit it now, allow it to be emitted lazily on its first use.
    return;
  }

  assert(0 && "Unknown storage class");
}

static std::string GetStaticDeclName(CodeGenFunction &CGF, const VarDecl &D,
                                     const char *Separator) {
  CodeGenModule &CGM = CGF.CGM;
  if (CGF.getContext().getLangOptions().CPlusPlus) {
    llvm::StringRef Name = CGM.getMangledName(&D);
    return Name.str();
  }
  
  std::string ContextName;
  if (!CGF.CurFuncDecl) {
    // Better be in a block declared in global scope.
    const NamedDecl *ND = cast<NamedDecl>(&D);
    const DeclContext *DC = ND->getDeclContext();
    if (const BlockDecl *BD = dyn_cast<BlockDecl>(DC)) {
      MangleBuffer Name;
      CGM.getBlockMangledName(GlobalDecl(), Name, BD);
      ContextName = Name.getString();
    }
    else
      assert(0 && "Unknown context for block static var decl");
  } else if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(CGF.CurFuncDecl)) {
    llvm::StringRef Name = CGM.getMangledName(FD);
    ContextName = Name.str();
  } else if (isa<ObjCMethodDecl>(CGF.CurFuncDecl))
    ContextName = CGF.CurFn->getName();
  else
    assert(0 && "Unknown context for static var decl");
  
  return ContextName + Separator + D.getNameAsString();
}

llvm::GlobalVariable *
CodeGenFunction::CreateStaticVarDecl(const VarDecl &D,
                                     const char *Separator,
                                     llvm::GlobalValue::LinkageTypes Linkage) {
  QualType Ty = D.getType();
  assert(Ty->isConstantSizeType() && "VLAs can't be static");

  std::string Name = GetStaticDeclName(*this, D, Separator);

  llvm::Type *LTy = CGM.getTypes().ConvertTypeForMem(Ty);
  llvm::GlobalVariable *GV =
    new llvm::GlobalVariable(CGM.getModule(), LTy,
                             Ty.isConstant(getContext()), Linkage,
                             CGM.EmitNullConstant(D.getType()), Name, 0,
                             D.isThreadSpecified(),
                             CGM.getContext().getTargetAddressSpace(Ty));
  GV->setAlignment(getContext().getDeclAlign(&D).getQuantity());
  if (Linkage != llvm::GlobalValue::InternalLinkage)
    GV->setVisibility(CurFn->getVisibility());
  return GV;
}

/// AddInitializerToStaticVarDecl - Add the initializer for 'D' to the
/// global variable that has already been created for it.  If the initializer
/// has a different type than GV does, this may free GV and return a different
/// one.  Otherwise it just returns GV.
llvm::GlobalVariable *
CodeGenFunction::AddInitializerToStaticVarDecl(const VarDecl &D,
                                               llvm::GlobalVariable *GV) {
  llvm::Constant *Init = CGM.EmitConstantExpr(D.getInit(), D.getType(), this);

  // If constant emission failed, then this should be a C++ static
  // initializer.
  if (!Init) {
    if (!getContext().getLangOptions().CPlusPlus)
      CGM.ErrorUnsupported(D.getInit(), "constant l-value expression");
    else if (Builder.GetInsertBlock()) {
      // Since we have a static initializer, this global variable can't 
      // be constant.
      GV->setConstant(false);

      EmitCXXGuardedInit(D, GV);
    }
    return GV;
  }

  // The initializer may differ in type from the global. Rewrite
  // the global to match the initializer.  (We have to do this
  // because some types, like unions, can't be completely represented
  // in the LLVM type system.)
  if (GV->getType()->getElementType() != Init->getType()) {
    llvm::GlobalVariable *OldGV = GV;
    
    GV = new llvm::GlobalVariable(CGM.getModule(), Init->getType(),
                                  OldGV->isConstant(),
                                  OldGV->getLinkage(), Init, "",
                                  /*InsertBefore*/ OldGV,
                                  D.isThreadSpecified(),
                           CGM.getContext().getTargetAddressSpace(D.getType()));
    GV->setVisibility(OldGV->getVisibility());
    
    // Steal the name of the old global
    GV->takeName(OldGV);
    
    // Replace all uses of the old global with the new global
    llvm::Constant *NewPtrForOldDecl =
    llvm::ConstantExpr::getBitCast(GV, OldGV->getType());
    OldGV->replaceAllUsesWith(NewPtrForOldDecl);
    
    // Erase the old global, since it is no longer used.
    OldGV->eraseFromParent();
  }
  
  GV->setInitializer(Init);
  return GV;
}

void CodeGenFunction::EmitStaticVarDecl(const VarDecl &D,
                                      llvm::GlobalValue::LinkageTypes Linkage) {
  llvm::Value *&DMEntry = LocalDeclMap[&D];
  assert(DMEntry == 0 && "Decl already exists in localdeclmap!");

  llvm::GlobalVariable *GV = CreateStaticVarDecl(D, ".", Linkage);

  // Store into LocalDeclMap before generating initializer to handle
  // circular references.
  DMEntry = GV;

  // We can't have a VLA here, but we can have a pointer to a VLA,
  // even though that doesn't really make any sense.
  // Make sure to evaluate VLA bounds now so that we have them for later.
  if (D.getType()->isVariablyModifiedType())
    EmitVariablyModifiedType(D.getType());
  
  // Local static block variables must be treated as globals as they may be
  // referenced in their RHS initializer block-literal expresion.
  CGM.setStaticLocalDeclAddress(&D, GV);

  // If this value has an initializer, emit it.
  if (D.getInit())
    GV = AddInitializerToStaticVarDecl(D, GV);

  GV->setAlignment(getContext().getDeclAlign(&D).getQuantity());

  // FIXME: Merge attribute handling.
  if (const AnnotateAttr *AA = D.getAttr<AnnotateAttr>()) {
    SourceManager &SM = CGM.getContext().getSourceManager();
    llvm::Constant *Ann =
      CGM.EmitAnnotateAttr(GV, AA,
                           SM.getInstantiationLineNumber(D.getLocation()));
    CGM.AddAnnotation(Ann);
  }

  if (const SectionAttr *SA = D.getAttr<SectionAttr>())
    GV->setSection(SA->getName());

  if (D.hasAttr<UsedAttr>())
    CGM.AddUsedGlobal(GV);

  // We may have to cast the constant because of the initializer
  // mismatch above.
  //
  // FIXME: It is really dangerous to store this in the map; if anyone
  // RAUW's the GV uses of this constant will be invalid.
  llvm::Type *LTy = CGM.getTypes().ConvertTypeForMem(D.getType());
  llvm::Type *LPtrTy =
    LTy->getPointerTo(CGM.getContext().getTargetAddressSpace(D.getType()));
  DMEntry = llvm::ConstantExpr::getBitCast(GV, LPtrTy);

  // Emit global variable debug descriptor for static vars.
  CGDebugInfo *DI = getDebugInfo();
  if (DI) {
    DI->setLocation(D.getLocation());
    DI->EmitGlobalVariable(static_cast<llvm::GlobalVariable *>(GV), &D);
  }
}

namespace {
  struct DestroyObject : EHScopeStack::Cleanup {
    DestroyObject(llvm::Value *addr, QualType type,
                  CodeGenFunction::Destroyer *destroyer,
                  bool useEHCleanupForArray)
      : addr(addr), type(type), destroyer(*destroyer),
        useEHCleanupForArray(useEHCleanupForArray) {}

    llvm::Value *addr;
    QualType type;
    CodeGenFunction::Destroyer &destroyer;
    bool useEHCleanupForArray;

    void Emit(CodeGenFunction &CGF, Flags flags) {
      // Don't use an EH cleanup recursively from an EH cleanup.
      bool useEHCleanupForArray =
        flags.isForNormalCleanup() && this->useEHCleanupForArray;

      CGF.emitDestroy(addr, type, destroyer, useEHCleanupForArray);
    }
  };

  struct DestroyNRVOVariable : EHScopeStack::Cleanup {
    DestroyNRVOVariable(llvm::Value *addr,
                        const CXXDestructorDecl *Dtor,
                        llvm::Value *NRVOFlag)
      : Dtor(Dtor), NRVOFlag(NRVOFlag), Loc(addr) {}

    const CXXDestructorDecl *Dtor;
    llvm::Value *NRVOFlag;
    llvm::Value *Loc;

    void Emit(CodeGenFunction &CGF, Flags flags) {
      // Along the exceptions path we always execute the dtor.
      bool NRVO = flags.isForNormalCleanup() && NRVOFlag;

      llvm::BasicBlock *SkipDtorBB = 0;
      if (NRVO) {
        // If we exited via NRVO, we skip the destructor call.
        llvm::BasicBlock *RunDtorBB = CGF.createBasicBlock("nrvo.unused");
        SkipDtorBB = CGF.createBasicBlock("nrvo.skipdtor");
        llvm::Value *DidNRVO = CGF.Builder.CreateLoad(NRVOFlag, "nrvo.val");
        CGF.Builder.CreateCondBr(DidNRVO, SkipDtorBB, RunDtorBB);
        CGF.EmitBlock(RunDtorBB);
      }
          
      CGF.EmitCXXDestructorCall(Dtor, Dtor_Complete,
                                /*ForVirtualBase=*/false, Loc);

      if (NRVO) CGF.EmitBlock(SkipDtorBB);
    }
  };

  struct CallStackRestore : EHScopeStack::Cleanup {
    llvm::Value *Stack;
    CallStackRestore(llvm::Value *Stack) : Stack(Stack) {}
    void Emit(CodeGenFunction &CGF, Flags flags) {
      llvm::Value *V = CGF.Builder.CreateLoad(Stack, "tmp");
      llvm::Value *F = CGF.CGM.getIntrinsic(llvm::Intrinsic::stackrestore);
      CGF.Builder.CreateCall(F, V);
    }
  };

  struct ExtendGCLifetime : EHScopeStack::Cleanup {
    const VarDecl &Var;
    ExtendGCLifetime(const VarDecl *var) : Var(*var) {}

    void Emit(CodeGenFunction &CGF, Flags flags) {
      // Compute the address of the local variable, in case it's a
      // byref or something.
      DeclRefExpr DRE(const_cast<VarDecl*>(&Var), Var.getType(), VK_LValue,
                      SourceLocation());
      llvm::Value *value = CGF.EmitLoadOfScalar(CGF.EmitDeclRefLValue(&DRE));
      CGF.EmitExtendGCLifetime(value);
    }
  };

  struct CallCleanupFunction : EHScopeStack::Cleanup {
    llvm::Constant *CleanupFn;
    const CGFunctionInfo &FnInfo;
    const VarDecl &Var;
    
    CallCleanupFunction(llvm::Constant *CleanupFn, const CGFunctionInfo *Info,
                        const VarDecl *Var)
      : CleanupFn(CleanupFn), FnInfo(*Info), Var(*Var) {}

    void Emit(CodeGenFunction &CGF, Flags flags) {
      DeclRefExpr DRE(const_cast<VarDecl*>(&Var), Var.getType(), VK_LValue,
                      SourceLocation());
      // Compute the address of the local variable, in case it's a byref
      // or something.
      llvm::Value *Addr = CGF.EmitDeclRefLValue(&DRE).getAddress();

      // In some cases, the type of the function argument will be different from
      // the type of the pointer. An example of this is
      // void f(void* arg);
      // __attribute__((cleanup(f))) void *g;
      //
      // To fix this we insert a bitcast here.
      QualType ArgTy = FnInfo.arg_begin()->type;
      llvm::Value *Arg =
        CGF.Builder.CreateBitCast(Addr, CGF.ConvertType(ArgTy));

      CallArgList Args;
      Args.add(RValue::get(Arg),
               CGF.getContext().getPointerType(Var.getType()));
      CGF.EmitCall(FnInfo, CleanupFn, ReturnValueSlot(), Args);
    }
  };
}

/// EmitAutoVarWithLifetime - Does the setup required for an automatic
/// variable with lifetime.
static void EmitAutoVarWithLifetime(CodeGenFunction &CGF, const VarDecl &var,
                                    llvm::Value *addr,
                                    Qualifiers::ObjCLifetime lifetime) {
  switch (lifetime) {
  case Qualifiers::OCL_None:
    llvm_unreachable("present but none");

  case Qualifiers::OCL_ExplicitNone:
    // nothing to do
    break;

  case Qualifiers::OCL_Strong: {
    CodeGenFunction::Destroyer &destroyer =
      (var.hasAttr<ObjCPreciseLifetimeAttr>()
       ? CodeGenFunction::destroyARCStrongPrecise
       : CodeGenFunction::destroyARCStrongImprecise);

    CleanupKind cleanupKind = CGF.getARCCleanupKind();
    CGF.pushDestroy(cleanupKind, addr, var.getType(), destroyer,
                    cleanupKind & EHCleanup);
    break;
  }
  case Qualifiers::OCL_Autoreleasing:
    // nothing to do
    break;
 
  case Qualifiers::OCL_Weak:
    // __weak objects always get EH cleanups; otherwise, exceptions
    // could cause really nasty crashes instead of mere leaks.
    CGF.pushDestroy(NormalAndEHCleanup, addr, var.getType(),
                    CodeGenFunction::destroyARCWeak,
                    /*useEHCleanup*/ true);
    break;
  }
}

static bool isAccessedBy(const VarDecl &var, const Stmt *s) {
  if (const Expr *e = dyn_cast<Expr>(s)) {
    // Skip the most common kinds of expressions that make
    // hierarchy-walking expensive.
    s = e = e->IgnoreParenCasts();

    if (const DeclRefExpr *ref = dyn_cast<DeclRefExpr>(e))
      return (ref->getDecl() == &var);
  }

  for (Stmt::const_child_range children = s->children(); children; ++children)
    // children might be null; as in missing decl or conditional of an if-stmt.
    if ((*children) && isAccessedBy(var, *children))
      return true;

  return false;
}

static bool isAccessedBy(const ValueDecl *decl, const Expr *e) {
  if (!decl) return false;
  if (!isa<VarDecl>(decl)) return false;
  const VarDecl *var = cast<VarDecl>(decl);
  return isAccessedBy(*var, e);
}

static void drillIntoBlockVariable(CodeGenFunction &CGF,
                                   LValue &lvalue,
                                   const VarDecl *var) {
  lvalue.setAddress(CGF.BuildBlockByrefAddress(lvalue.getAddress(), var));
}

void CodeGenFunction::EmitScalarInit(const Expr *init,
                                     const ValueDecl *D,
                                     LValue lvalue,
                                     bool capturedByInit) {
  Qualifiers::ObjCLifetime lifetime = lvalue.getObjCLifetime();
  if (!lifetime) {
    llvm::Value *value = EmitScalarExpr(init);
    if (capturedByInit)
      drillIntoBlockVariable(*this, lvalue, cast<VarDecl>(D));
    EmitStoreThroughLValue(RValue::get(value), lvalue);
    return;
  }

  // If we're emitting a value with lifetime, we have to do the
  // initialization *before* we leave the cleanup scopes.
  CodeGenFunction::RunCleanupsScope Scope(*this);
  if (const ExprWithCleanups *ewc = dyn_cast<ExprWithCleanups>(init))
    init = ewc->getSubExpr();

  // We have to maintain the illusion that the variable is
  // zero-initialized.  If the variable might be accessed in its
  // initializer, zero-initialize before running the initializer, then
  // actually perform the initialization with an assign.
  bool accessedByInit = false;
  if (lifetime != Qualifiers::OCL_ExplicitNone)
    accessedByInit = isAccessedBy(D, init);
  if (accessedByInit) {
    LValue tempLV = lvalue;
    // Drill down to the __block object if necessary.
    if (capturedByInit) {
      // We can use a simple GEP for this because it can't have been
      // moved yet.
      tempLV.setAddress(Builder.CreateStructGEP(tempLV.getAddress(),
                                   getByRefValueLLVMField(cast<VarDecl>(D))));
    }

    llvm::PointerType *ty
      = cast<llvm::PointerType>(tempLV.getAddress()->getType());
    ty = cast<llvm::PointerType>(ty->getElementType());

    llvm::Value *zero = llvm::ConstantPointerNull::get(ty);
    
    // If __weak, we want to use a barrier under certain conditions.
    if (lifetime == Qualifiers::OCL_Weak)
      EmitARCInitWeak(tempLV.getAddress(), zero);

    // Otherwise just do a simple store.
    else
      EmitStoreOfScalar(zero, tempLV);
  }

  // Emit the initializer.
  llvm::Value *value = 0;

  switch (lifetime) {
  case Qualifiers::OCL_None:
    llvm_unreachable("present but none");

  case Qualifiers::OCL_ExplicitNone:
    // nothing to do
    value = EmitScalarExpr(init);
    break;

  case Qualifiers::OCL_Strong: {
    value = EmitARCRetainScalarExpr(init);
    break;
  }

  case Qualifiers::OCL_Weak: {
    // No way to optimize a producing initializer into this.  It's not
    // worth optimizing for, because the value will immediately
    // disappear in the common case.
    value = EmitScalarExpr(init);

    if (capturedByInit) drillIntoBlockVariable(*this, lvalue, cast<VarDecl>(D));
    if (accessedByInit)
      EmitARCStoreWeak(lvalue.getAddress(), value, /*ignored*/ true);
    else
      EmitARCInitWeak(lvalue.getAddress(), value);
    return;
  }

  case Qualifiers::OCL_Autoreleasing:
    value = EmitARCRetainAutoreleaseScalarExpr(init);
    break;
  }

  if (capturedByInit) drillIntoBlockVariable(*this, lvalue, cast<VarDecl>(D));

  // If the variable might have been accessed by its initializer, we
  // might have to initialize with a barrier.  We have to do this for
  // both __weak and __strong, but __weak got filtered out above.
  if (accessedByInit && lifetime == Qualifiers::OCL_Strong) {
    llvm::Value *oldValue = EmitLoadOfScalar(lvalue);
    EmitStoreOfScalar(value, lvalue);
    EmitARCRelease(oldValue, /*precise*/ false);
    return;
  }

  EmitStoreOfScalar(value, lvalue);
}

/// EmitScalarInit - Initialize the given lvalue with the given object.
void CodeGenFunction::EmitScalarInit(llvm::Value *init, LValue lvalue) {
  Qualifiers::ObjCLifetime lifetime = lvalue.getObjCLifetime();
  if (!lifetime)
    return EmitStoreThroughLValue(RValue::get(init), lvalue);

  switch (lifetime) {
  case Qualifiers::OCL_None:
    llvm_unreachable("present but none");

  case Qualifiers::OCL_ExplicitNone:
    // nothing to do
    break;

  case Qualifiers::OCL_Strong:
    init = EmitARCRetain(lvalue.getType(), init);
    break;

  case Qualifiers::OCL_Weak:
    // Initialize and then skip the primitive store.
    EmitARCInitWeak(lvalue.getAddress(), init);
    return;

  case Qualifiers::OCL_Autoreleasing:
    init = EmitARCRetainAutorelease(lvalue.getType(), init);
    break;
  }

  EmitStoreOfScalar(init, lvalue);  
}

/// canEmitInitWithFewStoresAfterMemset - Decide whether we can emit the
/// non-zero parts of the specified initializer with equal or fewer than
/// NumStores scalar stores.
static bool canEmitInitWithFewStoresAfterMemset(llvm::Constant *Init,
                                                unsigned &NumStores) {
  // Zero and Undef never requires any extra stores.
  if (isa<llvm::ConstantAggregateZero>(Init) ||
      isa<llvm::ConstantPointerNull>(Init) ||
      isa<llvm::UndefValue>(Init))
    return true;
  if (isa<llvm::ConstantInt>(Init) || isa<llvm::ConstantFP>(Init) ||
      isa<llvm::ConstantVector>(Init) || isa<llvm::BlockAddress>(Init) ||
      isa<llvm::ConstantExpr>(Init))
    return Init->isNullValue() || NumStores--;

  // See if we can emit each element.
  if (isa<llvm::ConstantArray>(Init) || isa<llvm::ConstantStruct>(Init)) {
    for (unsigned i = 0, e = Init->getNumOperands(); i != e; ++i) {
      llvm::Constant *Elt = cast<llvm::Constant>(Init->getOperand(i));
      if (!canEmitInitWithFewStoresAfterMemset(Elt, NumStores))
        return false;
    }
    return true;
  }
  
  // Anything else is hard and scary.
  return false;
}

/// emitStoresForInitAfterMemset - For inits that
/// canEmitInitWithFewStoresAfterMemset returned true for, emit the scalar
/// stores that would be required.
static void emitStoresForInitAfterMemset(llvm::Constant *Init, llvm::Value *Loc,
                                         bool isVolatile, CGBuilderTy &Builder) {
  // Zero doesn't require any stores.
  if (isa<llvm::ConstantAggregateZero>(Init) ||
      isa<llvm::ConstantPointerNull>(Init) ||
      isa<llvm::UndefValue>(Init))
    return;
  
  if (isa<llvm::ConstantInt>(Init) || isa<llvm::ConstantFP>(Init) ||
      isa<llvm::ConstantVector>(Init) || isa<llvm::BlockAddress>(Init) ||
      isa<llvm::ConstantExpr>(Init)) {
    if (!Init->isNullValue())
      Builder.CreateStore(Init, Loc, isVolatile);
    return;
  }
  
  assert((isa<llvm::ConstantStruct>(Init) || isa<llvm::ConstantArray>(Init)) &&
         "Unknown value type!");
  
  for (unsigned i = 0, e = Init->getNumOperands(); i != e; ++i) {
    llvm::Constant *Elt = cast<llvm::Constant>(Init->getOperand(i));
    if (Elt->isNullValue()) continue;
    
    // Otherwise, get a pointer to the element and emit it.
    emitStoresForInitAfterMemset(Elt, Builder.CreateConstGEP2_32(Loc, 0, i),
                                 isVolatile, Builder);
  }
}


/// shouldUseMemSetPlusStoresToInitialize - Decide whether we should use memset
/// plus some stores to initialize a local variable instead of using a memcpy
/// from a constant global.  It is beneficial to use memset if the global is all
/// zeros, or mostly zeros and large.
static bool shouldUseMemSetPlusStoresToInitialize(llvm::Constant *Init,
                                                  uint64_t GlobalSize) {
  // If a global is all zeros, always use a memset.
  if (isa<llvm::ConstantAggregateZero>(Init)) return true;


  // If a non-zero global is <= 32 bytes, always use a memcpy.  If it is large,
  // do it if it will require 6 or fewer scalar stores.
  // TODO: Should budget depends on the size?  Avoiding a large global warrants
  // plopping in more stores.
  unsigned StoreBudget = 6;
  uint64_t SizeLimit = 32;
  
  return GlobalSize > SizeLimit && 
         canEmitInitWithFewStoresAfterMemset(Init, StoreBudget);
}


/// EmitAutoVarDecl - Emit code and set up an entry in LocalDeclMap for a
/// variable declaration with auto, register, or no storage class specifier.
/// These turn into simple stack objects, or GlobalValues depending on target.
void CodeGenFunction::EmitAutoVarDecl(const VarDecl &D) {
  AutoVarEmission emission = EmitAutoVarAlloca(D);
  EmitAutoVarInit(emission);
  EmitAutoVarCleanups(emission);
}

/// EmitAutoVarAlloca - Emit the alloca and debug information for a
/// local variable.  Does not emit initalization or destruction.
CodeGenFunction::AutoVarEmission
CodeGenFunction::EmitAutoVarAlloca(const VarDecl &D) {
  QualType Ty = D.getType();

  AutoVarEmission emission(D);

  bool isByRef = D.hasAttr<BlocksAttr>();
  emission.IsByRef = isByRef;

  CharUnits alignment = getContext().getDeclAlign(&D);
  emission.Alignment = alignment;

  // If the type is variably-modified, emit all the VLA sizes for it.
  if (Ty->isVariablyModifiedType())
    EmitVariablyModifiedType(Ty);

  llvm::Value *DeclPtr;
  if (Ty->isConstantSizeType()) {
    if (!Target.useGlobalsForAutomaticVariables()) {
      bool NRVO = getContext().getLangOptions().ElideConstructors && 
                  D.isNRVOVariable();

      // If this value is a POD array or struct with a statically
      // determinable constant initializer, there are optimizations we
      // can do.
      // TODO: we can potentially constant-evaluate non-POD structs and
      // arrays as long as the initialization is trivial (e.g. if they
      // have a non-trivial destructor, but not a non-trivial constructor).
      if (D.getInit() &&
          (Ty->isArrayType() || Ty->isRecordType()) && 
          (Ty.isPODType(getContext()) ||
           getContext().getBaseElementType(Ty)->isObjCObjectPointerType()) &&
          D.getInit()->isConstantInitializer(getContext(), false)) {

        // If the variable's a const type, and it's neither an NRVO
        // candidate nor a __block variable, emit it as a global instead.
        if (CGM.getCodeGenOpts().MergeAllConstants && Ty.isConstQualified() &&
            !NRVO && !isByRef) {
          EmitStaticVarDecl(D, llvm::GlobalValue::InternalLinkage);

          emission.Address = 0; // signal this condition to later callbacks
          assert(emission.wasEmittedAsGlobal());
          return emission;
        }

        // Otherwise, tell the initialization code that we're in this case.
        emission.IsConstantAggregate = true;
      }
      
      // A normal fixed sized variable becomes an alloca in the entry block,
      // unless it's an NRVO variable.
      llvm::Type *LTy = ConvertTypeForMem(Ty);
      
      if (NRVO) {
        // The named return value optimization: allocate this variable in the
        // return slot, so that we can elide the copy when returning this
        // variable (C++0x [class.copy]p34).
        DeclPtr = ReturnValue;
        
        if (const RecordType *RecordTy = Ty->getAs<RecordType>()) {
          if (!cast<CXXRecordDecl>(RecordTy->getDecl())->hasTrivialDestructor()) {
            // Create a flag that is used to indicate when the NRVO was applied
            // to this variable. Set it to zero to indicate that NRVO was not 
            // applied.
            llvm::Value *Zero = Builder.getFalse();
            llvm::Value *NRVOFlag = CreateTempAlloca(Zero->getType(), "nrvo");
            EnsureInsertPoint();
            Builder.CreateStore(Zero, NRVOFlag);
            
            // Record the NRVO flag for this variable.
            NRVOFlags[&D] = NRVOFlag;
            emission.NRVOFlag = NRVOFlag;
          }
        }
      } else {
        if (isByRef)
          LTy = BuildByRefType(&D);
        
        llvm::AllocaInst *Alloc = CreateTempAlloca(LTy);
        Alloc->setName(D.getNameAsString());

        CharUnits allocaAlignment = alignment;
        if (isByRef)
          allocaAlignment = std::max(allocaAlignment, 
              getContext().toCharUnitsFromBits(Target.getPointerAlign(0)));
        Alloc->setAlignment(allocaAlignment.getQuantity());
        DeclPtr = Alloc;
      }
    } else {
      // Targets that don't support recursion emit locals as globals.
      const char *Class =
        D.getStorageClass() == SC_Register ? ".reg." : ".auto.";
      DeclPtr = CreateStaticVarDecl(D, Class,
                                    llvm::GlobalValue::InternalLinkage);
    }
  } else {
    EnsureInsertPoint();

    if (!DidCallStackSave) {
      // Save the stack.
      llvm::Value *Stack = CreateTempAlloca(Int8PtrTy, "saved_stack");

      llvm::Value *F = CGM.getIntrinsic(llvm::Intrinsic::stacksave);
      llvm::Value *V = Builder.CreateCall(F);

      Builder.CreateStore(V, Stack);

      DidCallStackSave = true;

      // Push a cleanup block and restore the stack there.
      // FIXME: in general circumstances, this should be an EH cleanup.
      EHStack.pushCleanup<CallStackRestore>(NormalCleanup, Stack);
    }

    llvm::Value *elementCount;
    QualType elementType;
    llvm::tie(elementCount, elementType) = getVLASize(Ty);

    llvm::Type *llvmTy = ConvertTypeForMem(elementType);

    // Allocate memory for the array.
    llvm::AllocaInst *vla = Builder.CreateAlloca(llvmTy, elementCount, "vla");
    vla->setAlignment(alignment.getQuantity());

    DeclPtr = vla;
  }

  llvm::Value *&DMEntry = LocalDeclMap[&D];
  assert(DMEntry == 0 && "Decl already exists in localdeclmap!");
  DMEntry = DeclPtr;
  emission.Address = DeclPtr;

  // Emit debug info for local var declaration.
  if (HaveInsertPoint())
    if (CGDebugInfo *DI = getDebugInfo()) {
      DI->setLocation(D.getLocation());
      if (Target.useGlobalsForAutomaticVariables()) {
        DI->EmitGlobalVariable(static_cast<llvm::GlobalVariable *>(DeclPtr), &D);
      } else
        DI->EmitDeclareOfAutoVariable(&D, DeclPtr, Builder);
    }

  return emission;
}

/// Determines whether the given __block variable is potentially
/// captured by the given expression.
static bool isCapturedBy(const VarDecl &var, const Expr *e) {
  // Skip the most common kinds of expressions that make
  // hierarchy-walking expensive.
  e = e->IgnoreParenCasts();

  if (const BlockExpr *be = dyn_cast<BlockExpr>(e)) {
    const BlockDecl *block = be->getBlockDecl();
    for (BlockDecl::capture_const_iterator i = block->capture_begin(),
           e = block->capture_end(); i != e; ++i) {
      if (i->getVariable() == &var)
        return true;
    }

    // No need to walk into the subexpressions.
    return false;
  }

  for (Stmt::const_child_range children = e->children(); children; ++children)
    if (isCapturedBy(var, cast<Expr>(*children)))
      return true;

  return false;
}

/// \brief Determine whether the given initializer is trivial in the sense
/// that it requires no code to be generated.
static bool isTrivialInitializer(const Expr *Init) {
  if (!Init)
    return true;
  
  if (const CXXConstructExpr *Construct = dyn_cast<CXXConstructExpr>(Init))
    if (CXXConstructorDecl *Constructor = Construct->getConstructor())
      if (Constructor->isTrivial() &&
          Constructor->isDefaultConstructor() &&
          !Construct->requiresZeroInitialization())
        return true;
      
  return false;
}
void CodeGenFunction::EmitAutoVarInit(const AutoVarEmission &emission) {
  assert(emission.Variable && "emission was not valid!");

  // If this was emitted as a global constant, we're done.
  if (emission.wasEmittedAsGlobal()) return;

  const VarDecl &D = *emission.Variable;
  QualType type = D.getType();

  // If this local has an initializer, emit it now.
  const Expr *Init = D.getInit();

  // If we are at an unreachable point, we don't need to emit the initializer
  // unless it contains a label.
  if (!HaveInsertPoint()) {
    if (!Init || !ContainsLabel(Init)) return;
    EnsureInsertPoint();
  }

  // Initialize the structure of a __block variable.
  if (emission.IsByRef)
    emitByrefStructureInit(emission);

  if (isTrivialInitializer(Init))
    return;
  

  CharUnits alignment = emission.Alignment;

  // Check whether this is a byref variable that's potentially
  // captured and moved by its own initializer.  If so, we'll need to
  // emit the initializer first, then copy into the variable.
  bool capturedByInit = emission.IsByRef && isCapturedBy(D, Init);

  llvm::Value *Loc =
    capturedByInit ? emission.Address : emission.getObjectAddress(*this);

  if (!emission.IsConstantAggregate) {
    LValue lv = MakeAddrLValue(Loc, type, alignment.getQuantity());
    lv.setNonGC(true);
    return EmitExprAsInit(Init, &D, lv, capturedByInit);
  }

  // If this is a simple aggregate initialization, we can optimize it
  // in various ways.
  assert(!capturedByInit && "constant init contains a capturing block?");

  bool isVolatile = type.isVolatileQualified();

  llvm::Constant *constant = CGM.EmitConstantExpr(D.getInit(), type, this);
  assert(constant != 0 && "Wasn't a simple constant init?");

  llvm::Value *SizeVal =
    llvm::ConstantInt::get(IntPtrTy, 
                           getContext().getTypeSizeInChars(type).getQuantity());

  llvm::Type *BP = Int8PtrTy;
  if (Loc->getType() != BP)
    Loc = Builder.CreateBitCast(Loc, BP, "tmp");

  // If the initializer is all or mostly zeros, codegen with memset then do
  // a few stores afterward.
  if (shouldUseMemSetPlusStoresToInitialize(constant, 
                CGM.getTargetData().getTypeAllocSize(constant->getType()))) {
    Builder.CreateMemSet(Loc, llvm::ConstantInt::get(Int8Ty, 0), SizeVal,
                         alignment.getQuantity(), isVolatile);
    if (!constant->isNullValue()) {
      Loc = Builder.CreateBitCast(Loc, constant->getType()->getPointerTo());
      emitStoresForInitAfterMemset(constant, Loc, isVolatile, Builder);
    }
  } else {
    // Otherwise, create a temporary global with the initializer then 
    // memcpy from the global to the alloca.
    std::string Name = GetStaticDeclName(*this, D, ".");
    llvm::GlobalVariable *GV =
      new llvm::GlobalVariable(CGM.getModule(), constant->getType(), true,
                               llvm::GlobalValue::InternalLinkage,
                               constant, Name, 0, false, 0);
    GV->setAlignment(alignment.getQuantity());
    GV->setUnnamedAddr(true);
        
    llvm::Value *SrcPtr = GV;
    if (SrcPtr->getType() != BP)
      SrcPtr = Builder.CreateBitCast(SrcPtr, BP, "tmp");

    Builder.CreateMemCpy(Loc, SrcPtr, SizeVal, alignment.getQuantity(),
                         isVolatile);
  }
}

/// Emit an expression as an initializer for a variable at the given
/// location.  The expression is not necessarily the normal
/// initializer for the variable, and the address is not necessarily
/// its normal location.
///
/// \param init the initializing expression
/// \param var the variable to act as if we're initializing
/// \param loc the address to initialize; its type is a pointer
///   to the LLVM mapping of the variable's type
/// \param alignment the alignment of the address
/// \param capturedByInit true if the variable is a __block variable
///   whose address is potentially changed by the initializer
void CodeGenFunction::EmitExprAsInit(const Expr *init,
                                     const ValueDecl *D,
                                     LValue lvalue,
                                     bool capturedByInit) {
  QualType type = D->getType();

  if (type->isReferenceType()) {
    RValue rvalue = EmitReferenceBindingToExpr(init, D);
    if (capturedByInit) 
      drillIntoBlockVariable(*this, lvalue, cast<VarDecl>(D));
    EmitStoreThroughLValue(rvalue, lvalue);
  } else if (!hasAggregateLLVMType(type)) {
    EmitScalarInit(init, D, lvalue, capturedByInit);
  } else if (type->isAnyComplexType()) {
    ComplexPairTy complex = EmitComplexExpr(init);
    if (capturedByInit)
      drillIntoBlockVariable(*this, lvalue, cast<VarDecl>(D));
    StoreComplexToAddr(complex, lvalue.getAddress(), lvalue.isVolatile());
  } else if (type->isSliceType()) {
    SlicePairTy slice = EmitSliceExpr(init);
    if (capturedByInit)
      drillIntoBlockVariable(*this, lvalue, cast<VarDecl>(D));
    StoreSliceToAddr(slice, lvalue.getAddress(), lvalue.isVolatile());
  } else {
    // TODO: how can we delay here if D is captured by its initializer?
    EmitAggExpr(init, AggValueSlot::forLValue(lvalue, true, false));
  }
}

/// Enter a destroy cleanup for the given local variable.
void CodeGenFunction::emitAutoVarTypeCleanup(
                            const CodeGenFunction::AutoVarEmission &emission,
                            QualType::DestructionKind dtorKind) {
  assert(dtorKind != QualType::DK_none);

  // Note that for __block variables, we want to destroy the
  // original stack object, not the possibly forwarded object.
  llvm::Value *addr = emission.getObjectAddress(*this);

  const VarDecl *var = emission.Variable;
  QualType type = var->getType();

  CleanupKind cleanupKind = NormalAndEHCleanup;
  CodeGenFunction::Destroyer *destroyer = 0;

  switch (dtorKind) {
  case QualType::DK_none:
    llvm_unreachable("no cleanup for trivially-destructible variable");

  case QualType::DK_cxx_destructor:
    // If there's an NRVO flag on the emission, we need a different
    // cleanup.
    if (emission.NRVOFlag) {
      assert(!type->isArrayType());
      CXXDestructorDecl *dtor = type->getAsCXXRecordDecl()->getDestructor();
      EHStack.pushCleanup<DestroyNRVOVariable>(cleanupKind, addr, dtor,
                                               emission.NRVOFlag);
      return;
    }
    break;

  case QualType::DK_objc_strong_lifetime:
    // Suppress cleanups for pseudo-strong variables.
    if (var->isARCPseudoStrong()) return;
    
    // Otherwise, consider whether to use an EH cleanup or not.
    cleanupKind = getARCCleanupKind();

    // Use the imprecise destroyer by default.
    if (!var->hasAttr<ObjCPreciseLifetimeAttr>())
      destroyer = CodeGenFunction::destroyARCStrongImprecise;
    break;

  case QualType::DK_objc_weak_lifetime:
    break;
  }

  // If we haven't chosen a more specific destroyer, use the default.
  if (!destroyer) destroyer = &getDestroyer(dtorKind);

  // Use an EH cleanup in array destructors iff the destructor itself
  // is being pushed as an EH cleanup.
  bool useEHCleanup = (cleanupKind & EHCleanup);
  EHStack.pushCleanup<DestroyObject>(cleanupKind, addr, type, destroyer,
                                     useEHCleanup);
}

void CodeGenFunction::EmitAutoVarCleanups(const AutoVarEmission &emission) {
  assert(emission.Variable && "emission was not valid!");

  // If this was emitted as a global constant, we're done.
  if (emission.wasEmittedAsGlobal()) return;

  const VarDecl &D = *emission.Variable;

  // Check the type for a cleanup.
  if (QualType::DestructionKind dtorKind = D.getType().isDestructedType())
    emitAutoVarTypeCleanup(emission, dtorKind);

  // In GC mode, honor objc_precise_lifetime.
  if (getLangOptions().getGCMode() != LangOptions::NonGC &&
      D.hasAttr<ObjCPreciseLifetimeAttr>()) {
    EHStack.pushCleanup<ExtendGCLifetime>(NormalCleanup, &D);
  }

  // Handle the cleanup attribute.
  if (const CleanupAttr *CA = D.getAttr<CleanupAttr>()) {
    const FunctionDecl *FD = CA->getFunctionDecl();

    llvm::Constant *F = CGM.GetAddrOfFunction(FD);
    assert(F && "Could not find function!");

    const CGFunctionInfo &Info = CGM.getTypes().getFunctionInfo(FD);
    EHStack.pushCleanup<CallCleanupFunction>(NormalAndEHCleanup, F, &Info, &D);
  }

  // If this is a block variable, call _Block_object_destroy
  // (on the unforwarded address).
  if (emission.IsByRef)
    enterByrefCleanup(emission);
}

CodeGenFunction::Destroyer &
CodeGenFunction::getDestroyer(QualType::DestructionKind kind) {
  // This is surprisingly compiler-dependent.  GCC 4.2 can't bind
  // references to functions directly in returns, and using '*&foo'
  // confuses MSVC.  Luckily, the following code pattern works in both.
  Destroyer *destroyer = 0;
  switch (kind) {
  case QualType::DK_none: llvm_unreachable("no destroyer for trivial dtor");
  case QualType::DK_cxx_destructor:
    destroyer = &destroyCXXObject;
    break;
  case QualType::DK_objc_strong_lifetime:
    destroyer = &destroyARCStrongPrecise;
    break;
  case QualType::DK_objc_weak_lifetime:
    destroyer = &destroyARCWeak;
    break;
  }
  return *destroyer;
}

/// pushDestroy - Push the standard destructor for the given type.
void CodeGenFunction::pushDestroy(QualType::DestructionKind dtorKind,
                                  llvm::Value *addr, QualType type) {
  assert(dtorKind && "cannot push destructor for trivial type");

  CleanupKind cleanupKind = getCleanupKind(dtorKind);
  pushDestroy(cleanupKind, addr, type, getDestroyer(dtorKind),
              cleanupKind & EHCleanup);
}

void CodeGenFunction::pushDestroy(CleanupKind cleanupKind, llvm::Value *addr,
                                  QualType type, Destroyer &destroyer,
                                  bool useEHCleanupForArray) {
  pushFullExprCleanup<DestroyObject>(cleanupKind, addr, type,
                                     destroyer, useEHCleanupForArray);
}

/// emitDestroy - Immediately perform the destruction of the given
/// object.
///
/// \param addr - the address of the object; a type*
/// \param type - the type of the object; if an array type, all
///   objects are destroyed in reverse order
/// \param destroyer - the function to call to destroy individual
///   elements
/// \param useEHCleanupForArray - whether an EH cleanup should be
///   used when destroying array elements, in case one of the
///   destructions throws an exception
void CodeGenFunction::emitDestroy(llvm::Value *addr, QualType type,
                                  Destroyer &destroyer,
                                  bool useEHCleanupForArray) {
  const ArrayType *arrayType = getContext().getAsArrayType(type);
  if (!arrayType)
    return destroyer(*this, addr, type);

  llvm::Value *begin = addr;
  llvm::Value *length = emitArrayLength(arrayType, type, begin);

  // Normally we have to check whether the array is zero-length.
  bool checkZeroLength = true;

  // But if the array length is constant, we can suppress that.
  if (llvm::ConstantInt *constLength = dyn_cast<llvm::ConstantInt>(length)) {
    // ...and if it's constant zero, we can just skip the entire thing.
    if (constLength->isZero()) return;
    checkZeroLength = false;
  }

  llvm::Value *end = Builder.CreateInBoundsGEP(begin, length);
  emitArrayDestroy(begin, end, type, destroyer,
                   checkZeroLength, useEHCleanupForArray);
}

/// emitArrayDestroy - Destroys all the elements of the given array,
/// beginning from last to first.  The array cannot be zero-length.
///
/// \param begin - a type* denoting the first element of the array
/// \param end - a type* denoting one past the end of the array
/// \param type - the element type of the array
/// \param destroyer - the function to call to destroy elements
/// \param useEHCleanup - whether to push an EH cleanup to destroy
///   the remaining elements in case the destruction of a single
///   element throws
void CodeGenFunction::emitArrayDestroy(llvm::Value *begin,
                                       llvm::Value *end,
                                       QualType type,
                                       Destroyer &destroyer,
                                       bool checkZeroLength,
                                       bool useEHCleanup) {
  assert(!type->isArrayType());

  // The basic structure here is a do-while loop, because we don't
  // need to check for the zero-element case.
  llvm::BasicBlock *bodyBB = createBasicBlock("arraydestroy.body");
  llvm::BasicBlock *doneBB = createBasicBlock("arraydestroy.done");

  if (checkZeroLength) {
    llvm::Value *isEmpty = Builder.CreateICmpEQ(begin, end,
                                                "arraydestroy.isempty");
    Builder.CreateCondBr(isEmpty, doneBB, bodyBB);
  }

  // Enter the loop body, making that address the current address.
  llvm::BasicBlock *entryBB = Builder.GetInsertBlock();
  EmitBlock(bodyBB);
  llvm::PHINode *elementPast =
    Builder.CreatePHI(begin->getType(), 2, "arraydestroy.elementPast");
  elementPast->addIncoming(end, entryBB);

  // Shift the address back by one element.
  llvm::Value *negativeOne = llvm::ConstantInt::get(SizeTy, -1, true);
  llvm::Value *element = Builder.CreateInBoundsGEP(elementPast, negativeOne,
                                                   "arraydestroy.element");

  if (useEHCleanup)
    pushRegularPartialArrayCleanup(begin, element, type, destroyer);

  // Perform the actual destruction there.
  destroyer(*this, element, type);

  if (useEHCleanup)
    PopCleanupBlock();

  // Check whether we've reached the end.
  llvm::Value *done = Builder.CreateICmpEQ(element, begin, "arraydestroy.done");
  Builder.CreateCondBr(done, doneBB, bodyBB);
  elementPast->addIncoming(element, Builder.GetInsertBlock());

  // Done.
  EmitBlock(doneBB);
}

/// Perform partial array destruction as if in an EH cleanup.  Unlike
/// emitArrayDestroy, the element type here may still be an array type.
static void emitPartialArrayDestroy(CodeGenFunction &CGF,
                                    llvm::Value *begin, llvm::Value *end,
                                    QualType type,
                                    CodeGenFunction::Destroyer &destroyer) {
  // If the element type is itself an array, drill down.
  unsigned arrayDepth = 0;
  while (const ArrayType *arrayType = CGF.getContext().getAsArrayType(type)) {
    // VLAs don't require a GEP index to walk into.
    if (!isa<VariableArrayType>(arrayType))
      arrayDepth++;
    type = arrayType->getElementType();
  }

  if (arrayDepth) {
    llvm::Value *zero = llvm::ConstantInt::get(CGF.SizeTy, arrayDepth+1);

    llvm::SmallVector<llvm::Value*,4> gepIndices(arrayDepth, zero);
    begin = CGF.Builder.CreateInBoundsGEP(begin, gepIndices.begin(),
                                          gepIndices.end(), "pad.arraybegin");
    end = CGF.Builder.CreateInBoundsGEP(end, gepIndices.begin(),
                                        gepIndices.end(), "pad.arrayend");
  }

  // Destroy the array.  We don't ever need an EH cleanup because we
  // assume that we're in an EH cleanup ourselves, so a throwing
  // destructor causes an immediate terminate.
  CGF.emitArrayDestroy(begin, end, type, destroyer,
                       /*checkZeroLength*/ true, /*useEHCleanup*/ false);
}

namespace {
  /// RegularPartialArrayDestroy - a cleanup which performs a partial
  /// array destroy where the end pointer is regularly determined and
  /// does not need to be loaded from a local.
  class RegularPartialArrayDestroy : public EHScopeStack::Cleanup {
    llvm::Value *ArrayBegin;
    llvm::Value *ArrayEnd;
    QualType ElementType;
    CodeGenFunction::Destroyer &Destroyer;
  public:
    RegularPartialArrayDestroy(llvm::Value *arrayBegin, llvm::Value *arrayEnd,
                               QualType elementType,
                               CodeGenFunction::Destroyer *destroyer)
      : ArrayBegin(arrayBegin), ArrayEnd(arrayEnd),
        ElementType(elementType), Destroyer(*destroyer) {}

    void Emit(CodeGenFunction &CGF, Flags flags) {
      emitPartialArrayDestroy(CGF, ArrayBegin, ArrayEnd,
                              ElementType, Destroyer);
    }
  };

  /// IrregularPartialArrayDestroy - a cleanup which performs a
  /// partial array destroy where the end pointer is irregularly
  /// determined and must be loaded from a local.
  class IrregularPartialArrayDestroy : public EHScopeStack::Cleanup {
    llvm::Value *ArrayBegin;
    llvm::Value *ArrayEndPointer;
    QualType ElementType;
    CodeGenFunction::Destroyer &Destroyer;
  public:
    IrregularPartialArrayDestroy(llvm::Value *arrayBegin,
                                 llvm::Value *arrayEndPointer,
                                 QualType elementType,
                                 CodeGenFunction::Destroyer *destroyer)
      : ArrayBegin(arrayBegin), ArrayEndPointer(arrayEndPointer),
        ElementType(elementType), Destroyer(*destroyer) {}

    void Emit(CodeGenFunction &CGF, Flags flags) {
      llvm::Value *arrayEnd = CGF.Builder.CreateLoad(ArrayEndPointer);
      emitPartialArrayDestroy(CGF, ArrayBegin, arrayEnd,
                              ElementType, Destroyer);
    }
  };
}

/// pushIrregularPartialArrayCleanup - Push an EH cleanup to destroy
/// already-constructed elements of the given array.  The cleanup
/// may be popped with DeactivateCleanupBlock or PopCleanupBlock.
/// 
/// \param elementType - the immediate element type of the array;
///   possibly still an array type
/// \param array - a value of type elementType*
/// \param destructionKind - the kind of destruction required
/// \param initializedElementCount - a value of type size_t* holding
///   the number of successfully-constructed elements
void CodeGenFunction::pushIrregularPartialArrayCleanup(llvm::Value *arrayBegin,
                                                 llvm::Value *arrayEndPointer,
                                                       QualType elementType,
                                                       Destroyer &destroyer) {
  pushFullExprCleanup<IrregularPartialArrayDestroy>(EHCleanup,
                                                    arrayBegin, arrayEndPointer,
                                                    elementType, &destroyer);
}

/// pushRegularPartialArrayCleanup - Push an EH cleanup to destroy
/// already-constructed elements of the given array.  The cleanup
/// may be popped with DeactivateCleanupBlock or PopCleanupBlock.
/// 
/// \param elementType - the immediate element type of the array;
///   possibly still an array type
/// \param array - a value of type elementType*
/// \param destructionKind - the kind of destruction required
/// \param initializedElementCount - a value of type size_t* holding
///   the number of successfully-constructed elements
void CodeGenFunction::pushRegularPartialArrayCleanup(llvm::Value *arrayBegin,
                                                     llvm::Value *arrayEnd,
                                                     QualType elementType,
                                                     Destroyer &destroyer) {
  pushFullExprCleanup<RegularPartialArrayDestroy>(EHCleanup,
                                                  arrayBegin, arrayEnd,
                                                  elementType, &destroyer);
}

namespace {
  /// A cleanup to perform a release of an object at the end of a
  /// function.  This is used to balance out the incoming +1 of a
  /// ns_consumed argument when we can't reasonably do that just by
  /// not doing the initial retain for a __block argument.
  struct ConsumeARCParameter : EHScopeStack::Cleanup {
    ConsumeARCParameter(llvm::Value *param) : Param(param) {}

    llvm::Value *Param;

    void Emit(CodeGenFunction &CGF, Flags flags) {
      CGF.EmitARCRelease(Param, /*precise*/ false);
    }
  };
}

/// Emit an alloca (or GlobalValue depending on target)
/// for the specified parameter and set up LocalDeclMap.
void CodeGenFunction::EmitParmDecl(const VarDecl &D, llvm::Value *Arg,
                                   unsigned ArgNo) {
  // FIXME: Why isn't ImplicitParamDecl a ParmVarDecl?
  assert((isa<ParmVarDecl>(D) || isa<ImplicitParamDecl>(D)) &&
         "Invalid argument to EmitParmDecl");

  Arg->setName(D.getName());

  // Use better IR generation for certain implicit parameters.
  if (isa<ImplicitParamDecl>(D)) {
    // The only implicit argument a block has is its literal.
    if (BlockInfo) {
      LocalDeclMap[&D] = Arg;

      if (CGDebugInfo *DI = getDebugInfo()) {
        DI->setLocation(D.getLocation());
        DI->EmitDeclareOfBlockLiteralArgVariable(*BlockInfo, Arg, Builder);
      }

      return;
    }
  }

  QualType Ty = D.getType();

  llvm::Value *DeclPtr;
  // If this is an aggregate or variable sized value, reuse the input pointer.
  if (!Ty->isConstantSizeType() ||
      CodeGenFunction::hasAggregateLLVMType(Ty)) {
    DeclPtr = Arg;
  } else {
    // Otherwise, create a temporary to hold the value.
    DeclPtr = CreateMemTemp(Ty, D.getName() + ".addr");

    bool doStore = true;

    Qualifiers qs = Ty.getQualifiers();

    if (Qualifiers::ObjCLifetime lt = qs.getObjCLifetime()) {
      // We honor __attribute__((ns_consumed)) for types with lifetime.
      // For __strong, it's handled by just skipping the initial retain;
      // otherwise we have to balance out the initial +1 with an extra
      // cleanup to do the release at the end of the function.
      bool isConsumed = D.hasAttr<NSConsumedAttr>();

      // 'self' is always formally __strong, but if this is not an
      // init method then we don't want to retain it.
      if (D.isARCPseudoStrong()) {
        const ObjCMethodDecl *method = cast<ObjCMethodDecl>(CurCodeDecl);
        assert(&D == method->getSelfDecl());
        assert(lt == Qualifiers::OCL_Strong);
        assert(qs.hasConst());
        assert(method->getMethodFamily() != OMF_init);
        (void) method;
        lt = Qualifiers::OCL_ExplicitNone;
      }

      if (lt == Qualifiers::OCL_Strong) {
        if (!isConsumed)
          // Don't use objc_retainBlock for block pointers, because we
          // don't want to Block_copy something just because we got it
          // as a parameter.
          Arg = EmitARCRetainNonBlock(Arg);
      } else {
        // Push the cleanup for a consumed parameter.
        if (isConsumed)
          EHStack.pushCleanup<ConsumeARCParameter>(getARCCleanupKind(), Arg);

        if (lt == Qualifiers::OCL_Weak) {
          EmitARCInitWeak(DeclPtr, Arg);
          doStore = false; // The weak init is a store, no need to do two
        }
      }

      // Enter the cleanup scope.
      EmitAutoVarWithLifetime(*this, D, DeclPtr, lt);
    }

    // Store the initial value into the alloca.
    if (doStore) {
      LValue lv = MakeAddrLValue(DeclPtr, Ty,
                                 getContext().getDeclAlign(&D).getQuantity());
      EmitStoreOfScalar(Arg, lv);
    }
  }

  llvm::Value *&DMEntry = LocalDeclMap[&D];
  assert(DMEntry == 0 && "Decl already exists in localdeclmap!");
  DMEntry = DeclPtr;

  // Emit debug info for param declaration.
  if (CGDebugInfo *DI = getDebugInfo())
    DI->EmitDeclareOfArgVariable(&D, DeclPtr, ArgNo, Builder);
}
