Require Import Arith.

Fixpoint add n m := 
  match n with 0 => m | S p => add p (S m) end.

Theorem addnS: forall n m, add n (S m) = S (add n m).
Proof.
  induction n.
  
  intros m; simpl.
  reflexivity.
  intros m; simpl.
  apply IHn.
Qed.

(*
Lemma addn0 : forall n, add n 0 = n.
Proof.
 induction n.
 simpl.
 reflexivity.
 simpl.
 rewrite addnS.
 rewrite IHn.
 reflexivity.
Qed.
*)

Lemma add_comm : forall n m, add n m = add m n.
Proof.
 induction n;
 intros.
 simpl.
 assert (forall x, add x 0 = x).

 induction x.
 simpl.
 reflexivity.
 simpl.
 rewrite addnS.
 rewrite IHx.
 reflexivity.
 rewrite H.
 reflexivity.
 rewrite addnS.
 simpl.
 rewrite addnS.
 rewrite IHn.
 reflexivity.
Qed.
