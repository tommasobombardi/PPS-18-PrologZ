implicit def fromString(name: String): PzValidation[Term] = {
	val nameVal1: PzValidation[String] =
		if(name.nonEmpty) name.successNel
		else InputError("An empty string is not valid ...").failureNel
	val nameVal2: PzValidation[String] =
		if(name.toCharArray.forall(_.isLetter)) name.successNel
		else InputError(s"String '$name' is not valid ...").failureNel
	(nameVal1 |@| nameVal2)((name, _) =>
		if(name.charAt(0).isLower) Atom(name) else Variable(name))
}

def validateProgram(theory: List[PzValidation[Clause]],
	goals: List[PzValidation[Fact]]):
	PzValidation[(List[Clause], List[Fact])] =
(theory.foldLeft(List.empty[Clause].successNel[String@@InputError])
((accumulator, element) => (accumulator |@| element)
((acc, el) => acc :+ el))
|@| goals.foldLeft(List.empty[Fact].successNel[String@@InputError])
((accumulator, element) => (accumulator |@| element)
((acc, el) => acc :+ el)))((_, _))
