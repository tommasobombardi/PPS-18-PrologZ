package utils

import prologz.dsl.{AtomImpl, Clause, Fact, FactImpl, RuleImpl, StructImpl, VariableImpl}

/** Sample prolog programs used for testing */
trait PrologSamples {

  val mulTheory: List[Clause] = List(
    FactImpl("sum", List(VariableImpl("X"), AtomImpl(0), VariableImpl("X"))),
    RuleImpl(FactImpl("sum", List(VariableImpl("X"), StructImpl("s", List(VariableImpl("Y"))), StructImpl("s", List(VariableImpl("Z"))))),
      List(FactImpl("sum", List(VariableImpl("X"), VariableImpl("Y"), VariableImpl("Z"))))),
    FactImpl("mul", List(VariableImpl("X"), AtomImpl(0), AtomImpl(0))),
    RuleImpl(FactImpl("mul", List(VariableImpl("X"), StructImpl("s", List(VariableImpl("Y"))), VariableImpl("Z"))),
      List(FactImpl("mul", List(VariableImpl("X"), VariableImpl("Y"), VariableImpl("W"))), FactImpl("sum", List(VariableImpl("X"), VariableImpl("W"), VariableImpl("Z"))))))

  val mulGoals: List[Fact] = List(
    FactImpl("mul", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), VariableImpl("Y"))))

  val relTheory: List[Clause] = List(
    RuleImpl(FactImpl("son", List(VariableImpl("X"), VariableImpl("Y"))),
      List(FactImpl("father", List(VariableImpl("Y"), VariableImpl("X"))), FactImpl("male", List(VariableImpl("X"))))),
    FactImpl("father", List(AtomImpl("abraham"), AtomImpl("isaac"))),
    FactImpl("father", List(AtomImpl("terach"), AtomImpl("abraham"))),
    FactImpl("male", List(AtomImpl("isaac"))),
    FactImpl("male", List(AtomImpl("abraham"))),
    FactImpl("male", List(AtomImpl("terach"))))

  val relGoals: List[Fact] = List(
    FactImpl("son", List(VariableImpl("X"), VariableImpl("Y"))))

}
