//===--- CGExprSlice.cpp - Emit LLVM Code for Slice Exprs -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit Expr nodes with slice types as LLVM code.
//
//===----------------------------------------------------------------------===//

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtVisitor.h"
#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/ADT/SmallString.h"
using namespace clang;
using namespace CodeGen;

//===----------------------------------------------------------------------===//
//                        Slice Expression Emitter
//===----------------------------------------------------------------------===//

typedef CodeGenFunction::SlicePairTy SlicePairTy;

namespace  {
class SliceExprEmitter
  : public StmtVisitor<SliceExprEmitter, SlicePairTy> {
  CodeGenFunction &CGF;
  CGBuilderTy &Builder;
  // True is we should ignore the value of a
  bool IgnorePointer;
  bool IgnoreDims;
public:
  SliceExprEmitter(CodeGenFunction &cgf, bool i1=false, bool i2=false)
    : CGF(cgf), Builder(CGF.Builder), IgnorePointer(i1), IgnoreDims(i2) {
  }


  //===--------------------------------------------------------------------===//
  //                               Utilities
  //===--------------------------------------------------------------------===//

  bool TestAndClearIgnorePointer() {
    bool I = IgnorePointer;
    IgnorePointer = false;
    return I;
  }
  bool TestAndClearIgnoreDims() {
    bool I = IgnoreDims;
    IgnoreDims = false;
    return I;
  }

  /// EmitLoadOfLValue - Given an expression with slice type that represents a
  /// value l-value, this method emits the address of the l-value, then loads
  /// and returns the result.
  SlicePairTy EmitLoadOfLValue(const Expr *E) {
    return EmitLoadOfLValue(CGF.EmitLValue(E));
  }

  SlicePairTy EmitLoadOfLValue(LValue LV) {
    if (LV.isSimple())
      return EmitLoadOfSlice(LV.getAddress(), LV.isVolatileQualified());

    assert(LV.isPropertyRef() && "Unknown LValue type!");
    return CGF.EmitLoadOfPropertyRefLValue(LV).getSliceVal();
  }

  /// EmitLoadOfSlice - Given a pointer to a slice value, emit code to load
  /// the real and imaginary pieces.
  SlicePairTy EmitLoadOfSlice(llvm::Value *SrcPtr, bool isVolatile);

  /// EmitStoreThroughLValue - Given an l-value of slice type, store
  /// a slice number into it.
  void EmitStoreThroughLValue(SlicePairTy Val, LValue LV) {
    if (LV.isSimple())
      return EmitStoreOfSlice(Val, LV.getAddress(), LV.isVolatileQualified());

    assert(LV.isPropertyRef() && "Unknown LValue type!");
    CGF.EmitStoreThroughPropertyRefLValue(RValue::getSlice(Val), LV);
  }

  /// EmitStoreOfSlice - Store the specified real/imag parts into the
  /// specified value pointer.
  void EmitStoreOfSlice(SlicePairTy Val, llvm::Value *ResPtr, bool isVol);

  /// EmitSliceToSliceCast - Emit a cast from slice value Val to DestType.
  SlicePairTy EmitSliceToSliceCast(SlicePairTy Val, QualType SrcType,
                                   QualType DestType);

  //===--------------------------------------------------------------------===//
  //                            Visitor Methods
  //===--------------------------------------------------------------------===//

  SlicePairTy Visit(Expr *E) {
    return StmtVisitor<SliceExprEmitter, SlicePairTy>::Visit(E);
  }

  SlicePairTy VisitStmt(Stmt *S) {
    S->dump(CGF.getContext().getSourceManager());
    assert(0 && "Stmt can't have slice result type!");
    return SlicePairTy();
  }
  SlicePairTy VisitExpr(Expr *S);
  SlicePairTy VisitParenExpr(ParenExpr *PE) { return Visit(PE->getSubExpr());}
  SlicePairTy VisitGenericSelectionExpr(GenericSelectionExpr *GE) {
    return Visit(GE->getResultExpr());
  }

  // l-values.
  SlicePairTy VisitDeclRefExpr(const Expr *E) { return EmitLoadOfLValue(E); }
  SlicePairTy VisitObjCIvarRefExpr(ObjCIvarRefExpr *E) {
    return EmitLoadOfLValue(E);
  }
  SlicePairTy VisitObjCPropertyRefExpr(ObjCPropertyRefExpr *E) {
    assert(E->getObjectKind() == OK_Ordinary);
    return EmitLoadOfLValue(E);
  }
  SlicePairTy VisitObjCMessageExpr(ObjCMessageExpr *E) {
    return CGF.EmitObjCMessageExpr(E).getSliceVal();
  }
  SlicePairTy VisitArraySubscriptExpr(Expr *E) { return EmitLoadOfLValue(E); }
  SlicePairTy VisitMemberExpr(const Expr *E) { return EmitLoadOfLValue(E); }
  SlicePairTy VisitOpaqueValueExpr(OpaqueValueExpr *E) {
    if (E->isGLValue())
      return EmitLoadOfLValue(CGF.getOpaqueLValueMapping(E));
    return CGF.getOpaqueRValueMapping(E).getSliceVal();
  }

  // FIXME: CompoundLiteralExpr

  SlicePairTy EmitCast(CastExpr::CastKind CK, Expr *Op, QualType DestTy);
  SlicePairTy VisitImplicitCastExpr(ImplicitCastExpr *E) {
    // Unlike for scalars, we don't have to worry about function->ptr demotion
    // here.
    return EmitCast(E->getCastKind(), E->getSubExpr(), E->getType());
  }
  SlicePairTy VisitCastExpr(CastExpr *E) {
    return EmitCast(E->getCastKind(), E->getSubExpr(), E->getType());
  }
  SlicePairTy VisitSliceExpr(const SliceExpr *E);
  SlicePairTy VisitCallExpr(const CallExpr *E);
  SlicePairTy VisitStmtExpr(const StmtExpr *E);

  // Operators.
  SlicePairTy VisitPrePostIncDec(const UnaryOperator *E,
                                 bool isInc, bool isPre) {
    LValue LV = CGF.EmitLValue(E->getSubExpr());
    return CGF.EmitSlicePrePostIncDec(E, LV, isInc, isPre);
  }
  SlicePairTy VisitUnaryPostDec(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, false, false);
  }
  SlicePairTy VisitUnaryPostInc(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, true, false);
  }
  SlicePairTy VisitUnaryPreDec(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, false, true);
  }
  SlicePairTy VisitUnaryPreInc(const UnaryOperator *E) {
    return VisitPrePostIncDec(E, true, true);
  }
  SlicePairTy VisitUnaryDeref(const Expr *E) { return EmitLoadOfLValue(E); }
  SlicePairTy VisitUnaryPlus     (const UnaryOperator *E) {
    TestAndClearIgnorePointer();
    TestAndClearIgnoreDims();
    return Visit(E->getSubExpr());
  }
  SlicePairTy VisitUnaryMinus    (const UnaryOperator *E);
  SlicePairTy VisitUnaryNot      (const UnaryOperator *E);
  // LNot,Real,Imag never return slice.
  SlicePairTy VisitUnaryExtension(const UnaryOperator *E) {
    return Visit(E->getSubExpr());
  }
  SlicePairTy VisitCXXDefaultArgExpr(CXXDefaultArgExpr *DAE) {
    return Visit(DAE->getExpr());
  }
  SlicePairTy VisitExprWithCleanups(ExprWithCleanups *E) {
    return CGF.EmitExprWithCleanups(E).getSliceVal();
  }
  SlicePairTy VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr *E) {
    assert(E->getType()->isSliceType() && "Expected slice type!");
    QualType Elem = E->getType()->getAs<SliceType>()->getPointeeType();
    llvm::Constant *Null = llvm::Constant::getNullValue(CGF.ConvertType(Elem));
    return SlicePairTy(Null, Null);
  }
  SlicePairTy VisitImplicitValueInitExpr(ImplicitValueInitExpr *E) {
    assert(E->getType()->isSliceType() && "Expected slice type!");
    QualType Elem = E->getType()->getAs<SliceType>()->getPointeeType();
    llvm::Constant *Null =
                       llvm::Constant::getNullValue(CGF.ConvertType(Elem));
    return SlicePairTy(Null, Null);
  }

  struct BinOpInfo {
    SlicePairTy LHS;
    SlicePairTy RHS;
    QualType Ty;  // Computation Type.
  };

  BinOpInfo EmitBinOps(const BinaryOperator *E);
  LValue EmitCompoundAssignLValue(const CompoundAssignOperator *E,
                                  SlicePairTy (SliceExprEmitter::*Func)
                                  (const BinOpInfo &),
                                  SlicePairTy &Val);
  SlicePairTy EmitCompoundAssign(const CompoundAssignOperator *E,
                                 SlicePairTy (SliceExprEmitter::*Func)
                                 (const BinOpInfo &));

  SlicePairTy EmitBinAdd(const BinOpInfo &Op);
  SlicePairTy EmitBinSub(const BinOpInfo &Op);

  SlicePairTy VisitBinAdd(const BinaryOperator *E) {
    return EmitBinAdd(EmitBinOps(E));
  }
  SlicePairTy VisitBinSub(const BinaryOperator *E) {
    return EmitBinSub(EmitBinOps(E));
  }

  // Compound assignments.
  SlicePairTy VisitBinAddAssign(const CompoundAssignOperator *E) {
    return EmitCompoundAssign(E, &SliceExprEmitter::EmitBinAdd);
  }
  SlicePairTy VisitBinSubAssign(const CompoundAssignOperator *E) {
    return EmitCompoundAssign(E, &SliceExprEmitter::EmitBinSub);
  }

  // No comparisons produce a slice result.

  LValue EmitBinAssignLValue(const BinaryOperator *E,
                             SlicePairTy &Val);
  SlicePairTy VisitBinAssign     (const BinaryOperator *E);
  SlicePairTy VisitBinComma      (const BinaryOperator *E);


  SlicePairTy
  VisitAbstractConditionalOperator(const AbstractConditionalOperator *CO);
  SlicePairTy VisitChooseExpr(ChooseExpr *CE);

  SlicePairTy VisitInitListExpr(InitListExpr *E);

  SlicePairTy VisitVAArgExpr(VAArgExpr *E);
};
}  // end anonymous namespace.

//===----------------------------------------------------------------------===//
//                                Utilities
//===----------------------------------------------------------------------===//

/// EmitLoadOfSlice - Given an RValue reference for a slice, emit code to
/// load the real and imaginary pieces, returning them as Real/Imag.
SlicePairTy SliceExprEmitter::EmitLoadOfSlice(llvm::Value *SrcPtr,
                                              bool isVolatile) {
  llvm::Value *A=0, *B=0;

  if (!IgnorePointer || isVolatile) {
    llvm::Value *AP = Builder.CreateStructGEP(SrcPtr, 0,
                                              SrcPtr->getName() + ".ap");
    A = Builder.CreateLoad(AP, isVolatile, SrcPtr->getName() + ".a");
  }

  if (!IgnoreDims || isVolatile) {
    B = Builder.CreateStructGEP(SrcPtr, 1, SrcPtr->getName() + ".bp");
  }

  return SlicePairTy(A, B);
}

/// EmitStoreOfSlice - Store the specified parts into the
/// specified value pointer.
void SliceExprEmitter::EmitStoreOfSlice(SlicePairTy Val, llvm::Value *DestPtr,
                                        bool isVolatile) {
  llvm::Value *APtr = Builder.CreateStructGEP(DestPtr, 0,
                                              DestPtr->getName() + ".a.ptr");
  llvm::Value *BPtr = Builder.CreateStructGEP(DestPtr, 1,
                                              DestPtr->getName() + ".b.ptr");

  Builder.CreateStore(Val.first, APtr, isVolatile);

  if (Val.second->getType() == BPtr->getType()) {
    unsigned NumElements = cast<llvm::ArrayType>(
                             cast<llvm::PointerType>(Val.second->getType())
                             ->getElementType())
                           ->getNumElements();

    for (unsigned i = 0; i != NumElements; ++i) {
      llvm::Value *Addr = Builder.CreateStructGEP(Val.second, i);
      llvm::Value *I = Builder.CreateLoad(Addr, isVolatile); // FIXME: isVolatile only refers to Dest
      Addr = Builder.CreateStructGEP(BPtr, i);
      Builder.CreateStore(I, Addr, isVolatile);
    }
  } else
    Builder.CreateStore(Val.second, BPtr, isVolatile);
}

//===----------------------------------------------------------------------===//
//                            Visitor Methods
//===----------------------------------------------------------------------===//

SlicePairTy SliceExprEmitter::VisitExpr(Expr *E) {
  CGF.ErrorUnsupported(E, "slice expression");
  llvm::Type *EltTy =
    CGF.ConvertType(E->getType()->getAs<SliceType>()->getPointeeType());
  llvm::Value *U = llvm::UndefValue::get(EltTy);
  return SlicePairTy(U, U);
}

SlicePairTy SliceExprEmitter::VisitSliceExpr(const SliceExpr *E) {
  assert(E->getBase()->getType()->isPointerType() && "can only slice ptr");
  const SliceType *SliceTy = cast<SliceType>(E->getType());
  assert(SliceTy->getNumDims() == E->getNumArgs());

  // emit slice ptr
  llvm::Value *DataPtr = CGF.EmitScalarExpr(E->getBase());

  // emit slice dimensions
  unsigned NumDims = E->getNumArgs();
  llvm::SmallVector<llvm::Value *,8> Dims;
  for (unsigned i = 0, e = NumDims; i != e; ++i) {
    llvm::Value *Dim = CGF.EmitScalarExpr(E->getArg(i));
    Dims.push_back(Dim);
  }

  // calculate strides
  llvm::SmallVector<llvm::Value *,8> ReverseStrides;
  llvm::Value *Stride = Builder.getInt32(1);
  ReverseStrides.push_back(Stride);
  for (unsigned i = NumDims-1; i != 0; --i) {
    Stride = Builder.CreateMul(Stride, Dims[i]);
    ReverseStrides.push_back(Stride);
  }
  assert(ReverseStrides.size() == NumDims);

  // create tmp array
  unsigned ArrayLen = SliceTy->getArrayLen();
  //const llvm::Type *ITy = llvm::IntegerType::get(CGF.getLLVMContext(), 32); // FIXME
  llvm::ArrayType *ArrayTy = llvm::ArrayType::get(Builder.getInt32Ty(), ArrayLen);
  //llvm::Value *ArrayPtr = CGF.CreateMemTemp(ArrayTy, "slice.a.ptr");
  //llvm::AllocaInst(ArrayTy, 0, "");
  llvm::AllocaInst *ArrayTmp = Builder.CreateAlloca(ArrayTy,
                                               Builder.getInt32(1),
                                               "slice.arr.tmp");
  ArrayTmp->setAlignment(8);

  // fill array with strides
  llvm::Value *Address;
  for (unsigned i = 0; i != NumDims; ++i) {
    unsigned j = NumDims - 1 - i;
    Address = Builder.CreateStructGEP(ArrayTmp,
                                      SliceTy->getArrayIdxOfStride(i),
                                      "slice.arr.idx");
    Builder.CreateStore(ReverseStrides[j], Address);
  }

  // fill array with dimensions
  for (unsigned i = 0; i != NumDims; ++i) {
    Address = Builder.CreateStructGEP(ArrayTmp,
                                      SliceTy->getArrayIdxOfDim(i),
                                      "slice.arr.idx");
    Builder.CreateStore(Dims[i], Address);
  }

  return SlicePairTy(DataPtr, ArrayTmp);
}

SlicePairTy SliceExprEmitter::VisitCallExpr(const CallExpr *E) {
  if (E->getCallReturnType()->isReferenceType())
    return EmitLoadOfLValue(E);

  return CGF.EmitCallExpr(E).getSliceVal();
}

SlicePairTy SliceExprEmitter::VisitStmtExpr(const StmtExpr *E) {
  CodeGenFunction::StmtExprEvaluation eval(CGF);
  return CGF.EmitCompoundStmt(*E->getSubStmt(), true).getSliceVal();
}

/// EmitSliceToSliceCast - Emit a cast from slice value Val to DestType.
SlicePairTy SliceExprEmitter::EmitSliceToSliceCast(SlicePairTy Val,
                                                   QualType SrcType,
                                                   QualType DestType) {
  const SliceType *SrcST = SrcType->getAs<SliceType>();
  const SliceType *DestST = DestType->getAs<SliceType>();

  assert(SrcST->getNumDims() == DestST->getNumDims() && "num dims mismatch");

  // Convert the pointer
  Val.first = CGF.EmitScalarConversion(Val.first,
          CGF.getContext().getPointerType(SrcST->getPointeeType()),
          CGF.getContext().getPointerType(DestST->getPointeeType()));

  // Val.second is unmodified
  return Val;
}

SlicePairTy SliceExprEmitter::EmitCast(CastExpr::CastKind CK, Expr *Op,
                                       QualType DestTy) {
    /*
  switch (CK) {
  case CK_GetObjCProperty: {
    LValue LV = CGF.EmitLValue(Op);
    assert(LV.isPropertyRef() && "Unknown LValue type!");
    return CGF.EmitLoadOfPropertyRefLValue(LV).getSliceVal();
  }

  case CK_NoOp:
  case CK_LValueToRValue:
    return Visit(Op);

  // TODO: do all of these
  default:
    break;
  }

  // Two cases here: cast from (slice to slice) and (scalar to slice).
  if (Op->getType()->isSliceType())
    return EmitSliceToSliceCast(Visit(Op), Op->getType(), DestTy);

  // FIXME: We should be looking at all of the cast kinds here, not
  // cherry-picking the ones we have test cases for.
  if (CK == CK_LValueBitCast) {
    llvm::Value *V = CGF.EmitLValue(Op).getAddress();
    V = Builder.CreateBitCast(V,
                      CGF.ConvertType(CGF.getContext().getPointerType(DestTy)));
    // FIXME: Are the qualifiers correct here?
    return EmitLoadOfSlice(V, DestTy.isVolatileQualified());
  }

  // C99 6.3.1.7: When a value of real type is converted to a slice type, the
  // real part of the slice result value is determined by the rules of
  // conversion to the corresponding real type and the imaginary part of the
  // slice result value is a positive zero or an unsigned zero.
  llvm::Value *Elt = CGF.EmitScalarExpr(Op);

  // Convert the input element to the element type of the slice.
  DestTy = DestTy->getAs<SliceType>()->getPointeeType();
  Elt = CGF.EmitScalarConversion(Elt, Op->getType(), DestTy);

  // Return (realval, 0).
  return SlicePairTy(Elt, llvm::Constant::getNullValue(Elt->getType()));
  */

  switch (CK) {
  case CK_NoOp:
  case CK_LValueToRValue:
    return Visit(Op);

#if 0
  case CK_IntegralToSlice:
  case CK_PointerToSlice: {
    assert(Op->getType()->isPointerType() && "not ptr->slice?");

    // convert pointer type
    llvm::Value *Ptr = CGF.EmitScalarExpr(Op);
    const SliceType *ST = DestTy->getAs<SliceType>();
    Ptr = CGF.EmitScalarConversion(Ptr, Op->getType(),
            CGF.getContext().getPointerType(ST->getPointeeType())); // ???

    // construct array
    unsigned LenArray = ST->getArrayLen();
    const llvm::Type *ITy = llvm::IntegerType::get(CGF.getLLVMContext(), 32); // FIXME
    const llvm::ArrayType *ATy = llvm::ArrayType::get(ITy, LenArray);
    llvm::Constant *One = llvm::ConstantInt::get(ITy, 2);
    std::vector<llvm::Constant*> Fields(LenArray, One);
    llvm::Constant *Array = llvm::ConstantArray::get(ATy, Fields);

    return SlicePairTy(Ptr, Array);
  }
#endif

  case CK_SliceCast:
    return EmitSliceToSliceCast(Visit(Op), Op->getType(), DestTy);

  default:
    // FIXME
    break;
  }
}

SlicePairTy SliceExprEmitter::VisitUnaryMinus(const UnaryOperator *E) {
  TestAndClearIgnorePointer();
  TestAndClearIgnoreDims();
  SlicePairTy Op = Visit(E->getSubExpr());

  llvm::Value *ResR, *ResI;
  if (Op.first->getType()->isFloatingPointTy()) {
    ResR = Builder.CreateFNeg(Op.first,  "neg.r");
    ResI = Builder.CreateFNeg(Op.second, "neg.i");
  } else {
    ResR = Builder.CreateNeg(Op.first,  "neg.r");
    ResI = Builder.CreateNeg(Op.second, "neg.i");
  }
  return SlicePairTy(ResR, ResI);
}

SlicePairTy SliceExprEmitter::VisitUnaryNot(const UnaryOperator *E) {
  TestAndClearIgnorePointer();
  TestAndClearIgnoreDims();
  // ~(a+ib) = a + i*-b
  SlicePairTy Op = Visit(E->getSubExpr());
  llvm::Value *ResI;
  if (Op.second->getType()->isFloatingPointTy())
    ResI = Builder.CreateFNeg(Op.second, "conj.i");
  else
    ResI = Builder.CreateNeg(Op.second, "conj.i");

  return SlicePairTy(Op.first, ResI);
}

SlicePairTy SliceExprEmitter::EmitBinAdd(const BinOpInfo &Op) {
  llvm::Value *ResR, *ResI;

  if (Op.LHS.first->getType()->isFloatingPointTy()) {
    ResR = Builder.CreateFAdd(Op.LHS.first,  Op.RHS.first,  "add.r");
    ResI = Builder.CreateFAdd(Op.LHS.second, Op.RHS.second, "add.i");
  } else {
    ResR = Builder.CreateAdd(Op.LHS.first,  Op.RHS.first,  "add.r");
    ResI = Builder.CreateAdd(Op.LHS.second, Op.RHS.second, "add.i");
  }
  return SlicePairTy(ResR, ResI);
}

SlicePairTy SliceExprEmitter::EmitBinSub(const BinOpInfo &Op) {
  llvm::Value *ResR, *ResI;
  if (Op.LHS.first->getType()->isFloatingPointTy()) {
    ResR = Builder.CreateFSub(Op.LHS.first,  Op.RHS.first,  "sub.r");
    ResI = Builder.CreateFSub(Op.LHS.second, Op.RHS.second, "sub.i");
  } else {
    ResR = Builder.CreateSub(Op.LHS.first,  Op.RHS.first,  "sub.r");
    ResI = Builder.CreateSub(Op.LHS.second, Op.RHS.second, "sub.i");
  }
  return SlicePairTy(ResR, ResI);
}

SliceExprEmitter::BinOpInfo
SliceExprEmitter::EmitBinOps(const BinaryOperator *E) {
  TestAndClearIgnorePointer();
  TestAndClearIgnoreDims();
  BinOpInfo Ops;
  Ops.LHS = Visit(E->getLHS());
  Ops.RHS = Visit(E->getRHS());
  Ops.Ty = E->getType();
  return Ops;
}


LValue SliceExprEmitter::
EmitCompoundAssignLValue(const CompoundAssignOperator *E,
          SlicePairTy (SliceExprEmitter::*Func)(const BinOpInfo&),
                       SlicePairTy &Val) {
  TestAndClearIgnorePointer();
  TestAndClearIgnoreDims();
  QualType LHSTy = E->getLHS()->getType();

  BinOpInfo OpInfo;

  // Load the RHS and LHS operands.
  // __block variables need to have the rhs evaluated first, plus this should
  // improve codegen a little.
  OpInfo.Ty = E->getComputationResultType();

  // The RHS should have been converted to the computation type.
  assert(OpInfo.Ty->isSliceType());
  assert(CGF.getContext().hasSameUnqualifiedType(OpInfo.Ty,
                                                 E->getRHS()->getType()));
  OpInfo.RHS = Visit(E->getRHS());

  LValue LHS = CGF.EmitLValue(E->getLHS());

  // Load from the l-value.
  SlicePairTy LHSSlicePair = EmitLoadOfLValue(LHS);

  OpInfo.LHS = EmitSliceToSliceCast(LHSSlicePair, LHSTy, OpInfo.Ty);

  // Expand the binary operator.
  SlicePairTy Result = (this->*Func)(OpInfo);

  // Truncate the result back to the LHS type.
  Result = EmitSliceToSliceCast(Result, OpInfo.Ty, LHSTy);
  Val = Result;

  // Store the result value into the LHS lvalue.
  EmitStoreThroughLValue(Result, LHS);

  return LHS;
}

// Compound assignments.
SlicePairTy SliceExprEmitter::
EmitCompoundAssign(const CompoundAssignOperator *E,
                   SlicePairTy (SliceExprEmitter::*Func)(const BinOpInfo&)){
  SlicePairTy Val;
  LValue LV = EmitCompoundAssignLValue(E, Func, Val);

  // The result of an assignment in C is the assigned r-value.
  if (!CGF.getContext().getLangOptions().CPlusPlus)
    return Val;

  // Objective-C property assignment never reloads the value following a store.
  if (LV.isPropertyRef())
    return Val;

  // If the lvalue is non-volatile, return the computed value of the assignment.
  if (!LV.isVolatileQualified())
    return Val;

  return EmitLoadOfSlice(LV.getAddress(), LV.isVolatileQualified());
}

LValue SliceExprEmitter::EmitBinAssignLValue(const BinaryOperator *E,
                                             SlicePairTy &Val) {
  assert(CGF.getContext().hasSameUnqualifiedType(E->getLHS()->getType(),
                                                 E->getRHS()->getType()) &&
         "Invalid assignment");
  TestAndClearIgnorePointer();
  TestAndClearIgnoreDims();

  // Emit the RHS.  __block variables need the RHS evaluated first.
  Val = Visit(E->getRHS());

  // Compute the address to store into.
  LValue LHS = CGF.EmitLValue(E->getLHS());

  // Store the result value into the LHS lvalue.
  EmitStoreThroughLValue(Val, LHS);

  return LHS;
}

SlicePairTy SliceExprEmitter::VisitBinAssign(const BinaryOperator *E) {
  SlicePairTy Val;
  LValue LV = EmitBinAssignLValue(E, Val);

  // The result of an assignment in C is the assigned r-value.
  if (!CGF.getContext().getLangOptions().CPlusPlus)
    return Val;

  // Objective-C property assignment never reloads the value following a store.
  if (LV.isPropertyRef())
    return Val;

  // If the lvalue is non-volatile, return the computed value of the assignment.
  if (!LV.isVolatileQualified())
    return Val;

  return EmitLoadOfSlice(LV.getAddress(), LV.isVolatileQualified());
}

SlicePairTy SliceExprEmitter::VisitBinComma(const BinaryOperator *E) {
  CGF.EmitIgnoredExpr(E->getLHS());
  return Visit(E->getRHS());
}

SlicePairTy SliceExprEmitter::
VisitAbstractConditionalOperator(const AbstractConditionalOperator *E) {
  TestAndClearIgnorePointer();
  TestAndClearIgnoreDims();
  llvm::BasicBlock *LHSBlock = CGF.createBasicBlock("cond.true");
  llvm::BasicBlock *RHSBlock = CGF.createBasicBlock("cond.false");
  llvm::BasicBlock *ContBlock = CGF.createBasicBlock("cond.end");

  // Bind the common expression if necessary.
  CodeGenFunction::OpaqueValueMapping binding(CGF, E);

  CodeGenFunction::ConditionalEvaluation eval(CGF);
  CGF.EmitBranchOnBoolExpr(E->getCond(), LHSBlock, RHSBlock);

  eval.begin(CGF);
  CGF.EmitBlock(LHSBlock);
  SlicePairTy LHS = Visit(E->getTrueExpr());
  LHSBlock = Builder.GetInsertBlock();
  CGF.EmitBranch(ContBlock);
  eval.end(CGF);

  eval.begin(CGF);
  CGF.EmitBlock(RHSBlock);
  SlicePairTy RHS = Visit(E->getFalseExpr());
  RHSBlock = Builder.GetInsertBlock();
  CGF.EmitBlock(ContBlock);
  eval.end(CGF);

  // Create a PHI node for the real part.
  llvm::PHINode *RealPN = Builder.CreatePHI(LHS.first->getType(), 2, "cond.r");
  RealPN->addIncoming(LHS.first, LHSBlock);
  RealPN->addIncoming(RHS.first, RHSBlock);

  // Create a PHI node for the imaginary part.
  llvm::PHINode *ImagPN = Builder.CreatePHI(LHS.first->getType(), 2, "cond.i");
  ImagPN->addIncoming(LHS.second, LHSBlock);
  ImagPN->addIncoming(RHS.second, RHSBlock);

  return SlicePairTy(RealPN, ImagPN);
}

SlicePairTy SliceExprEmitter::VisitChooseExpr(ChooseExpr *E) {
  return Visit(E->getChosenSubExpr(CGF.getContext()));
}

SlicePairTy SliceExprEmitter::VisitInitListExpr(InitListExpr *E) {
    bool Ignore = TestAndClearIgnorePointer();
    (void)Ignore;
    assert (Ignore == false && "init list ignored");
    Ignore = TestAndClearIgnoreDims();
    (void)Ignore;
    assert (Ignore == false && "init list ignored");
  if (E->getNumInits())
    return Visit(E->getInit(0));

  // Empty init list intializes to null
  QualType Ty = E->getType()->getAs<SliceType>()->getPointeeType();
  llvm::Type* LTy = CGF.ConvertType(Ty);
  llvm::Value* zeroConstant = llvm::Constant::getNullValue(LTy);
  return SlicePairTy(zeroConstant, zeroConstant);
}

SlicePairTy SliceExprEmitter::VisitVAArgExpr(VAArgExpr *E) {
  llvm::Value *ArgValue = CGF.EmitVAListRef(E->getSubExpr());
  llvm::Value *ArgPtr = CGF.EmitVAArg(ArgValue, E->getType());

  if (!ArgPtr) {
    CGF.ErrorUnsupported(E, "slice va_arg expression");
    llvm::Type *EltTy =
      CGF.ConvertType(E->getType()->getAs<SliceType>()->getPointeeType());
    llvm::Value *U = llvm::UndefValue::get(EltTy);
    return SlicePairTy(U, U);
  }

  // FIXME Volatility.
  return EmitLoadOfSlice(ArgPtr, false);
}

//===----------------------------------------------------------------------===//
//                         Entry Point into this File
//===----------------------------------------------------------------------===//

/// EmitSliceExpr - Emit the computation of the specified expression of
/// slice type, ignoring the result.
SlicePairTy CodeGenFunction::EmitSliceExpr(const Expr *E, bool IgnorePointer,
                                           bool IgnoreDims) {
  assert(E && E->getType()->isSliceType() &&
         "Invalid slice expression to emit");

  return SliceExprEmitter(*this, IgnorePointer, IgnoreDims)
    .Visit(const_cast<Expr*>(E));
}

/// EmitSliceExprIntoAddr - Emit the computation of the specified expression
/// of slice type, storing into the specified Value*.
void CodeGenFunction::EmitSliceExprIntoAddr(const Expr *E,
                                            llvm::Value *DestAddr,
                                            bool DestIsVolatile) {
  assert(E && E->getType()->isSliceType() &&
         "Invalid slice expression to emit");
  SliceExprEmitter Emitter(*this);
  SlicePairTy Val = Emitter.Visit(const_cast<Expr*>(E));
  Emitter.EmitStoreOfSlice(Val, DestAddr, DestIsVolatile);
}

/// StoreSliceToAddr - Store a slice number into the specified address.
void CodeGenFunction::StoreSliceToAddr(SlicePairTy V,
                                       llvm::Value *DestAddr,
                                       bool DestIsVolatile) {
  SliceExprEmitter(*this).EmitStoreOfSlice(V, DestAddr, DestIsVolatile);
}

/// LoadSliceFromAddr - Load a slice number from the specified address.
SlicePairTy CodeGenFunction::LoadSliceFromAddr(llvm::Value *SrcAddr,
                                               bool SrcIsVolatile) {
  return SliceExprEmitter(*this).EmitLoadOfSlice(SrcAddr, SrcIsVolatile);
}

LValue CodeGenFunction::EmitSliceAssignmentLValue(const BinaryOperator *E) {
  assert(E->getOpcode() == BO_Assign);
  SlicePairTy Val; // ignored
  return SliceExprEmitter(*this).EmitBinAssignLValue(E, Val);
}

LValue CodeGenFunction::
EmitSliceCompoundAssignmentLValue(const CompoundAssignOperator *E) {
  SlicePairTy(SliceExprEmitter::*Op)(const SliceExprEmitter::BinOpInfo &);
  switch (E->getOpcode()) {
  case BO_SubAssign: Op = &SliceExprEmitter::EmitBinSub; break;
  case BO_AddAssign: Op = &SliceExprEmitter::EmitBinAdd; break;

  default:
    llvm_unreachable("unexpected slice compound assignment");
    Op = 0;
  }

  SlicePairTy Val; // ignored
  return SliceExprEmitter(*this).EmitCompoundAssignLValue(E, Op, Val);
}

SlicePairTy CodeGenFunction::
EmitSlicePrePostIncDec(const UnaryOperator *E, LValue LV,
                       bool isInc, bool isPre) {
  SlicePairTy InVal = LoadSliceFromAddr(LV.getAddress(),
                                            LV.isVolatileQualified());

  llvm::Value *NextVal;
  if (isa<llvm::IntegerType>(InVal.first->getType())) {
    uint64_t AmountVal = isInc ? 1 : -1;
    NextVal = llvm::ConstantInt::get(InVal.first->getType(), AmountVal, true);

    // Add the inc/dec to the real part.
    NextVal = Builder.CreateAdd(InVal.first, NextVal, isInc ? "inc" : "dec");
  } else {
    QualType ElemTy = E->getType()->getAs<SliceType>()->getPointeeType();
    llvm::APFloat FVal(getContext().getFloatTypeSemantics(ElemTy), 1);
    if (!isInc)
      FVal.changeSign();
    NextVal = llvm::ConstantFP::get(getLLVMContext(), FVal);

    // Add the inc/dec to the real part.
    NextVal = Builder.CreateFAdd(InVal.first, NextVal, isInc ? "inc" : "dec");
  }

  SlicePairTy IncVal(NextVal, InVal.second);

  // Store the updated result through the lvalue.
  StoreSliceToAddr(IncVal, LV.getAddress(), LV.isVolatileQualified());

  // If this is a postinc, return the value read from memory, otherwise use the
  // updated value.
  return isPre ? IncVal : InVal;
}

