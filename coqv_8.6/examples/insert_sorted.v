

Require Export List.
Export ListNotations.
Require Export Arith.
Require Import Coq.Sorting.Sorting.
Require Import Omega.


Theorem le_leb : (forall n m, n <=? m = true -> n <= m) /\ (forall n m, n <= m -> n <=? m = true).
Proof.
split.
(*begin prove the first case*)
induction n.
destruct m.
simpl.
intros.
apply le_0_n.
simpl.
intros.
apply le_0_n.
intros.
destruct m.
inversion H.
apply le_n_S.
apply IHn.
auto.
(*proved the first case, now prove the second case*)
induction n.
destruct m.
intros.
simpl.
reflexivity.
simpl.
intros.
reflexivity.
destruct m.
simpl.
intros.
inversion H.
simpl.
intros.
apply IHn.
apply le_S_n.
assumption.
(*end of the proof of the second case*)

(* (*the old proof goes from here.*)
split.
induction n; destruct m; simpl; auto using le_0_n, le_n_S.
intros.
inversion H.

induction n; destruct m; simpl; auto using le_S_n.
intros.
inversion H.
*)
Qed.

Theorem leb_false : forall  n m,
  n <=? m = false -> ~(n <= m).
Proof.
  induction n.
  simpl.
  intros.
  inversion H.
  destruct m.
  simpl.
  intros.
  auto using le_Sn_0.
  simpl.
  intros.
  apply IHn in H.
  omega.
Qed.

Lemma flip_not_le : forall (a b : nat), not (a <= b) -> b <= a.
Proof. intros. omega. Qed.

(* This is somewhat specific (and deliberately weak) lemma that turns
   up a lot in our main proof. *)
Lemma flip_not_leb : forall (a b : nat), (a <=? b) = false -> b <=? a = true.
Proof.
  intros. apply leb_false in H. apply flip_not_le in H.
  apply le_leb. assumption.
Qed.


Fixpoint insert_sorted (n : nat) (l : list nat) : list nat :=
  match l with
    | [] => [n]
    | n' :: l' =>
      if n <=? n'
      then n :: l
      else n' :: insert_sorted n l'
  end.

(*=========================================================================
  Proof: Insertion preserves sorting
*)
Hint Constructors Sorted.
Hint Constructors HdRel.

Theorem insert_sorted_stays_sorted : forall (n:nat) (l:list nat), Sorted le l -> Sorted le (insert_sorted n l).
Proof.
  induction l as [|n' l'].
  simpl.
  auto.
  simpl.
  auto.
  intros H_sorted_l.
  destruct (n <=? n') eqn:H_n_le_n'.
  apply Sorted_cons.
  assumption.
  apply HdRel_cons.
  apply le_leb.
  assumption.
  apply Sorted_inv in H_sorted_l.
  inversion H_sorted_l as [H_sorted_l' HdRel_n'_l'].
  apply Sorted_cons.
  apply IHl'. 
  assumption.
  apply IHl' in H_sorted_l'.
  destruct l'.
  simpl.
  apply HdRel_cons.
  apply le_leb.
  apply flip_not_leb.
  assumption.
  simpl.
  destruct (n <=? n0).
  apply HdRel_cons.
  apply le_leb.
  apply flip_not_leb.
  assumption.
  apply HdRel_cons.
  apply HdRel_inv in HdRel_n'_l'.
  assumption.
(* Qed. *)
