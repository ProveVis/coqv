(*
from SfLib.v
*)
(*
Copyright (c) 2012
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

(** * SfLib: Software Foundations Library *)

(** Here we collect together several useful definitions and theorems
    from Basics.v, List.v, Poly.v, Ind.v, and Logic.v that are not
    already in the Coq standard library.  From now on we can [Import]
    or [Export] this file, instead of cluttering our environment with
    all the examples and false starts in those files. *)

(** * From the Coq Standard Library *)

Require Omega.   (* needed for using the [omega] tactic *)
Require Export Bool.
Require Export List.
Export ListNotations.
Require Export Arith.
Require Export Arith.EqNat.  (* Contains [beq_nat], among other things *)

(** * From Basics.v *)

(* Definition admit {T: Type} : T.  Admitted. *)

Require String. 
Open Scope string_scope.

Ltac move_to_top x :=
  match reverse goal with
  | H : _ |- _ => try move x after H
  end.

Tactic Notation "assert_eq" ident(x) constr(v) :=
  let H := fresh in
  assert (x = v) as H by reflexivity;
  clear H.

Tactic Notation "Case_aux" ident(x) constr(name) :=
  first [
    set (x := name); move_to_top x
  | assert_eq x name; move_to_top x
  | fail 1 "because we are working on a different case" ].

Tactic Notation "Case" constr(name) := Case_aux Case name.
Tactic Notation "SCase" constr(name) := Case_aux SCase name.
Tactic Notation "SSCase" constr(name) := Case_aux SSCase name.
Tactic Notation "SSSCase" constr(name) := Case_aux SSSCase name.
Tactic Notation "SSSSCase" constr(name) := Case_aux SSSSCase name.
Tactic Notation "SSSSSCase" constr(name) := Case_aux SSSSSCase name.
Tactic Notation "SSSSSSCase" constr(name) := Case_aux SSSSSSCase name.
Tactic Notation "SSSSSSSCase" constr(name) := Case_aux SSSSSSSCase name.

Fixpoint ble_nat (n m : nat) : bool :=
  match n with
  | O => true
  | S n' =>
      match m with
      | O => false
      | S m' => ble_nat n' m'
      end
  end.

Theorem andb_true_elim1 : forall b c,
  andb b c = true -> b = true.
Proof.
  intros b c H.
  destruct b.
  Case "b = true".
    reflexivity.
  Case "b = false".
    rewrite <- H. reflexivity.  Qed.

Theorem andb_true_elim2 : forall b c,
  andb b c = true -> c = true.
Proof.
(* An exercise in Basics.v *)
Admitted.

Theorem beq_nat_sym : forall (n m : nat),
  beq_nat n m = beq_nat m n.
(* An exercise in Lists.v *)
Admitted.

(** * From Props.v *)

Inductive ev : nat -> Prop :=
  | ev_0 : ev O
  | ev_SS : forall n:nat, ev n -> ev (S (S n)).

(** * From Logic.v *)

Theorem andb_true : forall b c,
  andb b c = true -> b = true /\ c = true.
Proof.
  intros b c H.
  destruct b.
    destruct c.
      apply conj. reflexivity. reflexivity.
      inversion H.
    inversion H.  Qed.

Theorem false_beq_nat: forall n n' : nat,
     n <> n' ->
     beq_nat n n' = false.
Proof. 
(* An exercise in Logic.v *)
Admitted.

Theorem ex_falso_quodlibet : forall (P:Prop),
  False -> P.
Proof.
  intros P contra.
  inversion contra.  Qed.

Theorem ev_not_ev_S : forall n,
  ev n -> ~ ev (S n).
Proof. 
(* An exercise in Logic.v *)
Admitted.

Theorem ble_nat_true : forall n m,
  ble_nat n m = true -> n <= m.
(* An exercise in Logic.v *)
Admitted.

Theorem ble_nat_false : forall n m,
  ble_nat n m = false -> ~(n <= m).
(* An exercise in Logic.v *)
Admitted.

Inductive appears_in (n : nat) : list nat -> Prop :=
| ai_here : forall l, appears_in n (n::l)
| ai_later : forall m l, appears_in n l -> appears_in n (m::l).

Inductive next_nat (n:nat) : nat -> Prop :=
  | nn : next_nat n (S n).

Inductive total_relation : nat -> nat -> Prop :=
  tot : forall n m : nat, total_relation n m.

Inductive empty_relation : nat -> nat -> Prop := .

(** * From Later Files *)

Definition relation (X:Type) := X -> X -> Prop.

Definition deterministic {X: Type} (R: relation X) :=
  forall x y1 y2 : X, R x y1 -> R x y2 -> y1 = y2. 

Inductive multi (X:Type) (R: relation X) 
                            : X -> X -> Prop :=
  | multi_refl  : forall (x : X),
                 multi X R x x
  | multi_step : forall (x y z : X),
                    R x y ->
                    multi X R y z ->
                    multi X R x z.
Implicit Arguments multi [[X]]. 

Tactic Notation "multi_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "multi_refl" | Case_aux c "multi_step" ].

Theorem multi_R : forall (X:Type) (R:relation X) (x y : X),
       R x y -> multi R x y.
Proof.
  intros X R x y r.
  apply multi_step with y. apply r. apply multi_refl.   Qed.

Theorem multi_trans :
  forall (X:Type) (R: relation X) (x y z : X),
      multi R x y  ->
      multi R y z ->
      multi R x z.
Proof.
  (* FILL IN HERE *) Admitted.

(**  Identifiers and polymorphic partial maps. *)

Inductive id : Type := 
  Id : nat -> id.

Theorem eq_id_dec : forall id1 id2 : id, {id1 = id2} + {id1 <> id2}.
Proof.
   intros id1 id2.
   destruct id1 as [n1]. destruct id2 as [n2].
   destruct (eq_nat_dec n1 n2) as [Heq | Hneq].
   Case "n1 = n2".
     left. rewrite Heq. reflexivity.
   Case "n1 <> n2".
     right. intros contra. inversion contra. apply Hneq. apply H0.
Defined. 

Lemma eq_id : forall (T:Type) x (p q:T), 
              (if eq_id_dec x x then p else q) = p. 
Proof.
  intros. 
  destruct (eq_id_dec x x); try reflexivity. 
  apply ex_falso_quodlibet; auto.
Qed.

Lemma neq_id : forall (T:Type) x y (p q:T), x <> y -> 
               (if eq_id_dec x y then p else q) = q. 
Proof.
  (* FILL IN HERE *) Admitted.

Definition partial_map (A:Type) := id -> option A.

Definition empty {A:Type} : partial_map A := (fun _ => None). 

Notation "'\empty'" := empty.

Definition extend {A:Type} (Gamma : partial_map A) (x:id) (T : A) :=
  fun x' => if eq_id_dec x x' then Some T else Gamma x'.

Lemma extend_eq : forall A (ctxt: partial_map A) x T,
  (extend ctxt x T) x = Some T.
Proof.
  intros. unfold extend. rewrite eq_id; auto. 
Qed.

Lemma extend_neq : forall A (ctxt: partial_map A) x1 T x2,
  x2 <> x1 ->
  (extend ctxt x2 T) x1 = ctxt x1.
Proof.
  intros. unfold extend. rewrite neq_id; auto. 
Qed.

Lemma extend_shadow : forall A (ctxt: partial_map A) t1 t2 x1 x2,
  extend (extend ctxt x2 t1) x2 t2 x1 = extend ctxt x2 t2 x1.
Proof with auto.
  intros. unfold extend. destruct (eq_id_dec x2 x1).
  reflexivity.
  reflexivity.
Qed.

(** -------------------- *)

(** * Some useful tactics *)

Tactic Notation "solve_by_inversion_step" tactic(t) :=  
  match goal with  
  | H : _ |- _ => solve [ inversion H; subst; t ] 
  end
  || fail "because the goal is not solvable by inversion".

Tactic Notation "solve" "by" "inversion" "1" :=
  solve_by_inversion_step idtac.
Tactic Notation "solve" "by" "inversion" "2" :=
  solve_by_inversion_step (solve by inversion 1).
Tactic Notation "solve" "by" "inversion" "3" :=
  solve_by_inversion_step (solve by inversion 2).
Tactic Notation "solve" "by" "inversion" :=
  solve by inversion 1.

(** $Date: 2014-12-31 12:04:02 -0500 (Wed, 31 Dec 2014) $ *)



(*=========================================================================
  Setup & Sorting
*)

(*Require Import SfLib.*)
Require Import Coq.Structures.Orders.
Require Import Coq.Sorting.Sorting.
Require Import Omega.

(* From Coq.Structures.Orders. *)
Local Coercion is_true : bool >-> Sortclass.
Hint Unfold is_true.

(* From Coq.Sorting.Mergesort NatOrder example. Highest priority is 0. *)
Module Import PriorityOrder <: TotalLeBool.
  Definition t := nat.
  Fixpoint leb x y :=
    match x, y with
    | 0, _ => true
    | _, 0 => false
    | S x', S y' => leb x' y'
    end.
  Infix "<=?" := leb (*at level 35*).
  Theorem leb_total : forall a1 a2, a1 <=? a2 \/ a2 <=? a1.
  Proof. induction a1; destruct a2; simpl; auto. Qed.
End PriorityOrder.

(* Example test_Sorted : Sorted leb [1; 2; 3].
Proof. auto. Qed.

Example test_not_Sorted : Sorted leb [2; 1; 3] -> False.
Proof.
  (* Name the hypothesis on the left side of "->" and break Sorted down by cases. *)
  intros contra. inversion contra.

  (* Look for an obviously false hypothesis, break it down, and repeat. *)
  repeat match goal with
    | [ H : HdRel _ 2 [1; 3] |- False ] => inversion_clear H
    | [ H : is_true (2 <=? 1) |- False ] => inversion_clear H
  end.
Qed. *)


(*=========================================================================
  Theorems about inequality
  Much of this machinery exists to translate between boolean expressions
  like `leb a b` and logical propositions like `a <= b`.
*)

(* Deal with implications of the form `ObviouslyFalse -> P` by introducting
  `ObviouslyFalse` as a hypotheses and inverting it. There's probably a
  built-in which does this, but I can't find it. *)
Ltac antecedent_is_false :=
  (* Use solve to make sure we either prove our goal completely, or fail
     atomically and leave the hypotheses unchanged. *)
  solve [ intros contra; inversion contra ].

(*
Notation "'leb_true'" := (forall n m, n <=? m = true -> n <= m).
Notation "'le_implies_leb_true'" := (forall n m, n <= m -> n <=? m).
*)

Theorem le_leb : (forall n m, n <=? m = true -> n <= m) /\ (forall n m, n <= m -> n <=? m).
Proof.
split.

induction n; destruct m; simpl; auto using le_0_n, le_n_S.
intros.
inversion H.

induction n; destruct m; simpl; auto using le_S_n.
intros.
inversion H.
Qed.
(*
Theorem leb_true : forall n m,
  n <=? m = true -> n <= m.
Proof.
  (* Dispose of most cases mechanically using built-in hypotheses. *)
  induction n; destruct m; simpl; auto using le_0_n, le_n_S.
  (* The remaining case is an obvious contradiction. *)
  antecedent_is_false.
Qed.



Theorem le_implies_leb_true : forall n m, n <= m -> n <=? m.
Proof.
  induction n; destruct m; simpl; auto using le_S_n.
  antecedent_is_false.
Qed.
*)
(* Alternate: https://github.com/timjb/software-foundations/blob/master/Logic.v
   This gave me the hint to induct on m. *)
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
(*
  intros n m. generalize dependent n.
  (* Give the pattern of our proof, and simplify. *)
  induction m; destruct n; simpl;
    (* Eliminate cases with obvious contractions. *)
    try antecedent_is_false.
  Case "~ S n <= 0". auto using le_Sn_0.
  Case "n <=? m = false -> ~ S n <= S m".
     (* Use our induction hypothesis to rewrite the left-hand side,
        then use omega for logic crunching. *)
    intros H_not_leq_n_m. apply IHm in H_not_leq_n_m. 
    omega.
Qed.
*)
(*
Notation "'flip_not_le'" := (forall (a b : nat), not (a <= b) -> b <= a).
Notation "'flip_not_leb'" := (forall (a b : nat), (a <=? b) = false -> b <=? a).

Theorem flip_not_le_leb: flip_not_le /\ flip_not_leb.
Proof.
  split.
  intros.
  omega.
  intros.
  apply leb_false in H.
  assert (tmp_h : ((not (a<=b)) -> b <= a)).
  intros.
  omega.
  apply tmp_h in H.
  apply le_leb.
  assumption.
Qed.
*)
(* As a general rule, we can solve any trivial theorem about inequalities using
   the omega tactic. *)
Lemma flip_not_le : forall (a b : nat), not (a <= b) -> b <= a.
Proof. intros. omega. Qed.

(* This is somewhat specific (and deliberately weak) lemma that turns
   up a lot in our main proof. *)
Lemma flip_not_leb : forall (a b : nat), (a <=? b) = false -> b <=? a.
Proof.
  intros. apply leb_false in H. apply flip_not_le in H.
  apply le_leb. assumption.
Qed.


(*=========================================================================
  Insertion
*)

Fixpoint insert_sorted (n : nat) (l : list nat) : list nat :=
  match l with
    | [] => [n]
    | n' :: l' =>
      if n <=? n'
      then n :: l
      else n' :: insert_sorted n l'
  end.

(* It's worth writing unit tests before trying to prove something really
   complicated, because doing so will make basic failures obvious, and
   you won't watch a proof fall apart mysteriously on some subclause. *)
(* Example test_insert_3_1_2 :
  insert_sorted 2 (insert_sorted 1 (insert_sorted 3 [])) = [1; 2; 3].
Proof. reflexivity. Qed.

Example test_insert_2_1_3 :
  insert_sorted 3 (insert_sorted 1 (insert_sorted 2 [])) = [1; 2; 3].
Proof. reflexivity. Qed.

Example test_insert_2_1_1 :
  insert_sorted 1 (insert_sorted 1 (insert_sorted 2 [])) = [1; 1; 2].
Proof. reflexivity. Qed. *)


(*=========================================================================
  Proof: Insertion preserves sorting
*)

Hint Resolve flip_not_leb.
Hint Constructors Sorted.
Hint Constructors HdRel.

Theorem insert_sorted_stays_sorted : forall n l, Sorted leb l -> Sorted leb (insert_sorted n l).
Proof.
  (*intros n l H_sorted_l.*)
  induction l as [|n' l'].
  simpl.
  auto.
  simpl.
  auto.
  intros H_sorted_l.
  (*induction l as [|n' l']; 
simpl; auto.*)
  Case "l = n' :: l'".
    destruct (n <=? n') eqn:H_n_le_n'; auto.
    SCase "n <=? n' = false".
      apply Sorted_inv in H_sorted_l.
      inversion H_sorted_l as [H_sorted_l' HdRel_n'_l'].

      apply Sorted_cons.
      SSCase "Sorted (insert_sorted n l')". apply IHl'. auto.
      SSCase "HdRel n' (insert_sorted n l')".
        apply IHl' in H_sorted_l'.
        destruct l'; simpl; auto.
        destruct (n <=? n0); auto.
        inversion HdRel_n'_l'. 
        auto.
Qed.

