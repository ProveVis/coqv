Require Import Arith.

Fixpoint add n m := 
  match n with 0 => m | S p => add p (S m) end.

Lemma addnS : forall n m, add n (S m) = S (add n m).
induction n.
  intros m; simpl.
  reflexivity.
intros m; simpl.
now apply IHn.
Qed.