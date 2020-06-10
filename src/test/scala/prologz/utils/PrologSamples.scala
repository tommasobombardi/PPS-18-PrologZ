package prologz.utils

import prologz.dsl.{Atom, Clause, Fact, Rule, Struct, Variable}

/** Sample prolog programs used for testing */
trait PrologSamples {

  val mulTheory: List[Clause] = List(
    Fact("sum", List(Variable("X"), Atom(0), Variable("X"))),
    Rule(Fact("sum", List(Variable("X"), Struct("s", List(Variable("Y"))), Struct("s", List(Variable("Z"))))),
      List(Fact("sum", List(Variable("X"), Variable("Y"), Variable("Z"))))),
    Fact("mul", List(Variable("X"), Atom(0), Atom(0))),
    Rule(Fact("mul", List(Variable("X"), Struct("s", List(Variable("Y"))), Variable("Z"))),
      List(Fact("mul", List(Variable("X"), Variable("Y"), Variable("W"))), Fact("sum", List(Variable("X"), Variable("W"), Variable("Z"))))))

  val mulGoals: List[Fact] = List(
    Fact("mul", List(Struct("s", List(Struct("s", List(Atom(0))))), Struct("s", List(Struct("s", List(Atom(0))))), Variable("Y"))))

  val relTheory: List[Clause] = List(
    Rule(Fact("son", List(Variable("X"), Variable("Y"))),
      List(Fact("father", List(Variable("Y"), Variable("X"))), Fact("male", List(Variable("X"))))),
    Fact("father", List(Atom("abraham"), Atom("isaac"))),
    Fact("father", List(Atom("terach"), Atom("abraham"))),
    Fact("male", List(Atom("isaac"))),
    Fact("male", List(Atom("abraham"))),
    Fact("male", List(Atom("terach"))))

  val relGoals: List[Fact] = List(
    Fact("son", List(Variable("X"), Variable("Y"))))

}
