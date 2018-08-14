
(*Helper functions*)
Fixpoint minus_dot (m:nat) (n:nat) : nat :=
match m,n with
| 0,_ => 0
| _,0 => m
| S p,S q => minus_dot p q
end.
Definition If (cond:bool) (exp1:nat) (exp2:nat) := if cond then exp1 else exp2.
Definition orb (cond1:bool) (cond2:bool) := if cond1 then true else cond2.
Definition zerob (exp:nat) := 
match exp with
| 0 => true
| _ => false
end.

Definition upd1 (i1:nat) (r1:nat) (r3:bool) : nat := If r3 i1 (minus_dot r1 1).
Definition upd2 (i1:nat) (i2:nat) (r2:nat) (r3:bool) : nat := 
  If r3 (If (zerob i1) 0 i2) ((If (zerob i1) 0 i2) + r2).
Definition upd3 (i1:nat) (i2:nat) (r1:nat) (r3:bool) : bool := 
  orb (zerob (If r3 (minus_dot i1 1) (minus_dot (minus_dot r1 1) 1))) (zerob i2).





(*Registers, Input, and Output*)
Record TR:Set := reg {reg1:nat; reg2:nat; reg3:bool}.
Record TI:Set := inp {inp1:nat; inp2:nat}.
Record TO:Set := out {res:nat; done:bool}.


Definition update (i:TI) (r:TR) : TR :=
  (reg (upd1 (inp1 i) (reg1 r) (reg3 r))
      (upd2 (inp1 i) (inp2 i) (reg2 r) (reg3 r))
      (upd3 (inp1 i) (inp2 i) (reg1 r) (reg3 r))).
Definition output (i: TI) (r: TR) : TO := (out (reg2 r) (reg3 r)).


Definition IfProp (A:Prop) (B:Prop) (b:bool) : Prop := (b=true->A)/\(b=false->B).

Fixpoint mul (X:nat) (Y:nat) : nat :=
  match X,Y with
  | 0,_ => 0
  | _,0 => 0
  | S m,_ => Y + (mul m Y)
  end.

Definition InvM (X:nat) (Y:nat) (i:TI) (n:nat) (r:TR) : Prop :=
  (i=(inp X Y)) ->
  (IfProp (n <> 0 -> (reg2 r = mul X Y))
         ((minus_dot (reg1 r) 1 <> 0) /\ (X <> 0) /\ (mul (minus_dot (reg1 r) 1) Y + (reg2 r) = mul X Y))  
         (reg3 r)).

Definition Q (X:nat) (Y:nat) (i:TI) (n:nat) (o:TO) : Prop :=
  (i=inp X Y) -> n <> 0 -> done o = true -> (res o)=mul X Y.

Proposition correct : 
  forall X:nat, forall Y:nat, forall n:nat, forall r:TR,
  InvM X Y (inp X Y) n r ->
  (Q X Y (inp X Y) n (output (inp X Y) r)) /\ (InvM X Y (inp X Y) (S n) (update (inp X Y) r)).

Proof.
induction X.
intros.
split.










