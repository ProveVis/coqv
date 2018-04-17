Require Import Arith.
Fixpoint add n m := match n with 0 => m | S p => add p (S m) end.
Theorem addnS: forall n m, add n (S m) = S (add n m).
Proof.
induction n.
intros m; simpl.
reflexivity.
intros m; simpl.
now apply IHn.
Qed.

