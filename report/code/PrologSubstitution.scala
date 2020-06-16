implicit object SubstitutionMonoid extends Monoid[Substitution] {
 override val zero:Substitution= Substitution()
 override def append(s1:Substitution,s2:Substitution):Substitution=
  s1.keySet.zip(s1.values.toList.substitute(s2)).toMap
}
