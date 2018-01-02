Variables P Q R : Prop.
Lemma id_PP : (P -> P) -> P -> P.
Proof.
intros H p.
assumption.
Qed.

Lemma imp_dist : (P -> Q -> R) -> (P -> Q) -> P -> R.
Proof.
intros.
apply H.
assumption.
apply H0.
assumption.
Qed.

Lemma test: P -> Q -> R -> P.
Proof.
intros.
rename H0 into q.
clear q.
exact H.
Qed.

