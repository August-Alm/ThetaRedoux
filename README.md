## Theta Calculus

This repo contains an interpreter of a novel extension of
the lambda calculus that I've dubbed the "theta calculus".
Many people have studied questions related to writing a
meta-circular evaluator in the lambda calculus, i.e., how
one may go about writing an interpreter for the lambda
calculus in the calculus itself. The present calculus arose
from similar concerns, but where the problem instead is
to write a type checker of the calculus, in the calculus
itself. More on this semantics below.

Here is it's syntax:
```
t, u :=  x      // variable
      | λx.t    // lambda abstraction
      | (t u)   // application
      | θx.t    // theta abstraction
      | [t u]   // annotation
```
In what follows, we shall freely and implicitly assume the
standard conventions for variable renaming and variable
substitutions from treatments of the lambda calculus.

### Reduction rules

Here are the small-step reduction rules:

1. `(λx.t u) → t{x = u}`
2. `(θx.t u) → θx.(t [x u])`
3. `([t v] u) → [(t v) (u v)]`
4. `[t θx.u] → u{x = t}`
5. `[t λx.u] → λx.[(t x) u]`
6. `[[t a] b] when a ≃ b → t`

Rule (1) is the usual beta reduction of lambda calculus. The
rule (4) is exactly analogous, but involves the new theta binder
and the annotation pairing. The most crucial law is (6). The
equivalence relation it involves is that induced by consistent
variable renaming (alpha equivalence), the above reduction rules
(beta equivalence), and the following eta equivalences:

* `λx.(t x) ≃ t`
* `θx.[x t] ≃ t`

We have not fixed a reduction order. The interpreter loosely
follows a call-by-need strategy (though what that exactly means
in the present context is unclear).

### Informal semantics

The annotation pairing `[t u]` should be read as saying that
that `t` has type `u`, or that `t` is checked against `u`. The
theta abstraction thus creates a type, creating a rule for
how to check terms against that type. Let's give an example.
Assume `a` and `b` are some types, and define:
```
a → b := θf.λx.[(f [x a]) b]
```
Then
```
[f a → b] → λx.[(f [x a]) b]
```
We claim that this models the usual function type `a → b`. To
check that `f : a → b` with a given typing context one would
ordinarily check that `(f x)` has type `b` if `x : a` is added
to the context. The inner annotation `[x a]` serves the purpose
of the assumption, and the outer annotation `[... b]` serves
the purpose of the check. To continue the example, let's say
`f = λy.y`. We then have
```
[f a → b] → λx.[(λy.y [x a]) b] → λx.[([x a]) b]
```
Assume `a` and `b` are variables. Then we discern two possibilies:
either `a ≃ b`, in which case the expression reduces to `λx.x`,
by (6), or, it has reached normal form. This suggests the following
slogan:

_A traditional typing judgment `t : u` amounts to having an
equivalence `t ≃ [t u]`. In more detail, let `Γ = {x1 : a1, ..., xn : an}`
be a typing context. Define_
```
u{Γ} := u{x1 = [x1 a1], ..., xn = [xn an]}
```
_Then `Γ ⊢ t : u` if and only if `t{Γ} ≃ [t u]{Γ}`._

We hope to prove that one can faithfully encode polymorphic
System F operationally in the theta calculus this way. Arrow
types are encoded as above and polymorphic `∀a.t` as `λa.t`
(identifying `t` with its interpretation).

The calculus allows encoding also dependent function types:
```
Πx:a.(b x) := θf.λx.[(f [x a]) (b x)]
```
A trivial but still interesting example is `any := θt.t`.
Every term has that type. Types can surely be very exotic.

### Recursive extension

The interpreter currently extends the calculus with recursion
in the form of an additional fix-point binder `μx.t`, and
reduction rules

7. `(μx.t u) → (t{x = μx.t} u)`
8. `[μx.t u] → μx.[t{x = [x u]} u]`

This work is tentative and still buggy.