module Theta.Program

open Theta.Core
open Theta.Syntax

let showNormal (s : string) =
  Syntax.parseString s
  |> Syntax.term
  |> Term.normalize
  |> Term.show
  |> printfn "%s"

let equiv (a : string) (b : string) =
  let nf s = norm (hoas [] (Syntax.term (Syntax.parseString s)))
  let t = nf a
  let u = nf b
  let a = Term.show (quote t)
  let b = Term.show (quote u)
  if not (eq 0 t u) then
    printfn "Fail! %s ≄ %s" a b
  else
    printfn "Ok! %s ≃ %s" a b

[<EntryPoint>]
let main _ =
  showNormal
    "let pred = λn.λa.λs.λz.((((n a) λg.λh.(h (g s))) λu.z) λu.u);
     let three = λa.λs.λz.(s (s (s z)));
     (pred three)"
  showNormal
    "let true = λa.λt.λf.t;
     let false = λa.λt.λf.f;
     let is_zero = λn.λa.(((n a) λx.(false a)) (true a)); 
     let pred = λn.λa.λs.λz.((((n a) λg.λh.(h (g s))) λu.z) λu.u);
     let mul = λm.λn.λa.λs.λz.(((m a) ((n a) s)) z);
     let one = λa.λs.λz.(s z);
     let fact = μf.λn.λa.((((is_zero n) a) (one a)) (((mul n) (f (pred n))) a));
     let three = λa.λs.λz.(s (s (s z)));
     (fact three)"
  equiv
    "let one = λa.λs.λz.(s z);
     one"
    "let end = λb.θf.λy.[(f [y b]) b]; # ∀b.b → b
     let nat = λc.(end (end c)); # ∀c.(c → c) → c → c
     let one = λa.λs.λz.(s z);
     [one nat]"
  equiv
    "let mul = λm.λn.λa.λs.λz.(((m a) ((n a) s)) z);
     let one = λa.λs.λz.(s z);
     (mul one)"
    "let mul = λm.λn.λa.λs.λz.(((m a) ((n a) s)) z);
     let one = λa.λs.λz.(s z);
     let end = λb.θf.λy.[(f [y b]) b];
     let nat = λc.(end (end c));
     [(mul one) (end nat)]"
  equiv
    "let true = λt.λf.t;
     let false = λt.λf.f;
     let is_zero = λn.((n λx.false) true); 
     let one = λa.λs.λz.(s z);
     let end = λb.θf.λy.[(f [y b]) b];
     let nat = λc.(end (end c));
     (is_zero one)"
    "let true = λt.λf.t;
     let false = λt.λf.f;
     let is_zero = λn.((n λx.false) true); 
     let one = λa.λs.λz.(s z);
     let end = λb.θf.λy.[(f [y b]) b];
     let nat = λc.(end (end c));
     (is_zero [one nat])"
  equiv
    "let true = λa.λt.λf.t;
     let false = λa.λt.λf.f;
     let is_zero = λn.λa.(((n a) λx.(false a)) (true a)); 
     let pred = λn.λa.λs.λz.((((n a) λg.λh.(h (g s))) λu.z) λu.u);
     let mul = λm.λn.λa.λs.λz.(((m a) ((n a) s)) z);
     let one = λa.λs.λz.(s z);
     let fact = μf.λn.λa.((((is_zero n) a) (one a)) (((mul n) (f (pred n))) a));
     fact"
    "let true = λa.λt.λf.t;
     let false = λa.λt.λf.f;
     let is_zero = λn.λa.(((n a) λx.(false a)) (true a)); 
     let pred = λn.λa.λs.λz.((((n a) λg.λh.(h (g s))) λu.z) λu.u);
     let mul = λm.λn.λa.λs.λz.(((m a) ((n a) s)) z);
     let one = λa.λs.λz.(s z);
     let fact = μf.λn.λa.((((is_zero n) a) (one a)) (((mul n) (f (pred n))) a));
     let end = λb.θf.λy.[(f [y b]) b];
     let nat = λc.(end (end c));
     [fact (end nat)]"

  0

(*
PatientCharacteristicsNameOfInitials -> "Name or initials", not "Initials"

Case narrative ->  Describe suspected adverse reaction(s).

"Drug role" ???

"Drug form" needs adding!
DrugInformationsDosageInformationsPharmaceuticalDoseForm
before "Dosage"
*)