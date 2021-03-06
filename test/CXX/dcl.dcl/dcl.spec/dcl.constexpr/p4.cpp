// RUN: %clang_cc1 -verify -std=c++0x -fcxx-exceptions %s

namespace N {
  typedef char C;
}

namespace M {
  typedef double D;
}

struct NonLiteral { // expected-note 2{{no constexpr constructors}}
  NonLiteral() {}
  NonLiteral(int) {}
};
struct Literal {
  constexpr Literal() {}
  operator int() const { return 0; }
};

// Note, the wording applies constraints to the definition of constexpr
// constructors, but we intentionally apply all that we can to the declaration
// instead. See DR1360.

// In the definition of a constexpr constructor, each of the parameter types
// shall be a literal type.
struct S {
  constexpr S(int, N::C);
  constexpr S(int, NonLiteral, N::C); // expected-error {{constexpr constructor's 2nd parameter type 'NonLiteral' is not a literal type}}
  constexpr S(int, NonLiteral = 42); // expected-error {{constexpr constructor's 2nd parameter type 'NonLiteral' is not a literal type}}

  // In addition, either its function-body shall be = delete or = default
  constexpr S() = default;
  constexpr S(Literal) = delete;
};

// or it shall satisfy the following constraints:

// - the class shall not have any virtual base classes;
struct T : virtual S { // expected-note {{here}}
  constexpr T(); // expected-error {{constexpr constructor not allowed in struct with virtual base classes}}
};
namespace IndirectVBase {
  struct A {};
  struct B : virtual A {}; // expected-note {{here}}
  class C : public B {
  public:
    constexpr C(); // expected-error {{constexpr constructor not allowed in class with virtual base classes}}
  };
}

// - its function-body shall not be a function-try-block;
struct U {
  constexpr U()
    try // expected-error {{function try block not allowed in constexpr constructor}}
    : u() {
  } catch (...) {
    throw;
  }
  int u;
};

// - the compound-statememt of its function-body shall contain only
struct V {
  constexpr V() {
    //  - null statements,
    ;

    //  - static_assert-declarations,
    static_assert(true, "the impossible happened!");

    //  - typedef declarations and alias-declarations that do not define classes
    //    or enumerations,
    typedef int I;
    typedef struct S T;
    using J = int;
    using K = int[sizeof(I) + sizeof(J)];
    // Note, the standard requires we reject this.
    struct U;

    //  - using-declarations,
    using N::C;

    //  - and using-directives;
    using namespace N;
  }

  constexpr V(int(&)[1]) {
    for (int n = 0; n < 10; ++n) // expected-error {{statement not allowed in constexpr constructor}}
      /**/;
  }
  constexpr V(int(&)[2]) {
    constexpr int a = 0; // expected-error {{variables cannot be declared in a constexpr constructor}}
  }
  constexpr V(int(&)[3]) {
    constexpr int ForwardDecl(int); // expected-error {{statement not allowed in constexpr constructor}}
  }
  constexpr V(int(&)[4]) {
    typedef struct { } S1; // expected-error {{types cannot be defined in a constexpr constructor}}
  }
  constexpr V(int(&)[5]) {
    using S2 = struct { }; // expected-error {{types cannot be defined in a constexpr constructor}}
  }
  constexpr V(int(&)[6]) {
    struct S3 { }; // expected-error {{types cannot be defined in a constexpr constructor}}
  }
  constexpr V(int(&)[7]) {
    return; // expected-error {{statement not allowed in constexpr constructor}}
  }
};

// - every non-static data member and base class sub-object shall be initialized
struct W {
  int n; // expected-note {{member not initialized by constructor}}
  constexpr W() {} // expected-error {{constexpr constructor must initialize all members}}
};
struct AnonMembers {
  int a; // expected-note {{member not initialized by constructor}}
  union { // expected-note 2{{member not initialized by constructor}}
    char b;
    struct {
      double c;
      long d; // expected-note {{member not initialized by constructor}}
    };
    union {
      char e;
      void *f;
    };
  };
  struct { // expected-note {{member not initialized by constructor}}
    long long g;
    struct {
      int h; // expected-note {{member not initialized by constructor}}
      double i; // expected-note {{member not initialized by constructor}}
    };
    union { // expected-note 2{{member not initialized by constructor}}
      char *j;
      AnonMembers *k;
    };
  };

  constexpr AnonMembers(int(&)[1]) : a(), b(), g(), h(), i(), j() {} // ok
  // missing d, i, j/k union
  constexpr AnonMembers(int(&)[2]) : a(), c(), g(), h() {} // expected-error {{constexpr constructor must initialize all members}}
  constexpr AnonMembers(int(&)[3]) : a(), e(), g(), h(), i(), k() {} // ok
  // missing h, j/k union
  constexpr AnonMembers(int(&)[4]) : a(), c(), d(), g(), i() {} // expected-error {{constexpr constructor must initialize all members}}
  // missing b/c/d/e/f union
  constexpr AnonMembers(int(&)[5]) : a(), g(), h(), i(), k() {} // expected-error {{constexpr constructor must initialize all members}}
  // missing a, b/c/d/e/f union, g/h/i/j/k struct
  constexpr AnonMembers(int(&)[6]) {} // expected-error {{constexpr constructor must initialize all members}}
};

// - every constructor involved in initializing non-static data members and base
//   class sub-objects shall be a constexpr constructor.
//
// FIXME: Implement this as part of the 'must be able to produce a constant
// expression' rules.

// - every assignment-expression that is an initializer-caluse appearing
//   directly or indirectly within a brace-or-equal-initializer for a non-static
//   data member that is not named by a mem-initializer-id shall be a constant
//   expression; and
//
// Note, we deliberately do not implement this bullet, so that we can allow the
// following example. (See N3308).
struct X {
  int a = 0;
  int b = 2 * a + 1; // ok, not a constant expression.

  constexpr X() {}
  constexpr X(int c) : a(c) {} // ok, b initialized by 2 * c + 1
};

//  - every implicit conversion used in converting a constructor argument to the
//    corresponding parameter type and converting a full-expression to the
//    corresponding member type shall be one of those allowed in a constant
//    expression.
//
// We implement the proposed resolution of DR1364 and ignore this bullet.


namespace StdExample {
  struct Length {
    explicit constexpr Length(int i = 0) : val(i) { }
  private:
      int val;
  };
}
