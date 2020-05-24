package prologz.utils

import prologz.dsl.{AtomImpl, Clause, Fact, FactImpl, Predicate, RuleImpl, Struct, StructImpl, VariableImpl}
import prologz.dsl.ClauseImplicits._
import prologz.dsl.TermImplicits._
import prologz.resolution.Validation.PzValidation

trait Utils {

  val mulTheory: List[Clause] = List(
    FactImpl("sum", List(VariableImpl("X"), AtomImpl(0), VariableImpl("X"))),
    RuleImpl(FactImpl("sum", List(VariableImpl("X"), StructImpl("s", List(VariableImpl("Y"))), StructImpl("s", List(VariableImpl("Z"))))),
      List(FactImpl("sum", List(VariableImpl("X"), VariableImpl("Y"), VariableImpl("Z"))))),
    FactImpl("mul", List(VariableImpl("X"), AtomImpl(0), AtomImpl(0))),
    RuleImpl(FactImpl("mul", List(VariableImpl("X"), StructImpl("s", List(VariableImpl("Y"))), VariableImpl("Z"))),
      List(FactImpl("mul", List(VariableImpl("X"), VariableImpl("Y"), VariableImpl("W"))), FactImpl("sum", List(VariableImpl("X"), VariableImpl("W"), VariableImpl("Z"))))))

  val mulGoals: List[Fact] = List(FactImpl("mul", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), VariableImpl("Y"))))

  val mulTheoryNoErrors: List[PzValidation[Clause]] = List(
    Predicate("sum")("X", 0, "X"),
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")) :- Predicate("sum")("X", "Y", "Z"),
    Predicate("mul")("X", 0, 0),
    Predicate("mul")("X", Struct("s")("Y"), "Z") :- (Predicate("mul")("X", "Y", "W"), Predicate("sum")("X", "W", "Z")))

  val mulGoalsNoErrors: List[PzValidation[Fact]] = List(Predicate("mul")(Struct("s")(Struct("s")(0)), Struct("s")(Struct("s")(0)), "Y"))

  val mulTheoryWithErrors: List[PzValidation[Clause]] = List(
    Predicate("Sum")("X4", 0, "X"),
    Predicate("sum")("X", Struct("Ss")("Y"), Struct("s")("Z++")) :- Predicate("sum")("X", "Y.", "Z"),
    Predicate("mul")("X", 0, 0),
    Predicate("mul")("X", Struct("s")(":Y"), "Z") :- (Predicate("Mul")("X", "Y", "W"), Predicate("sum")("Xè", "W", "Z"))) // 7 errors

  val mulGoalsWithErrors: List[PzValidation[Fact]] = List(Predicate("Mul")(Struct("s")(Struct("s1")(0)), Struct("S")(Struct("s")(0)), "Y_")) // 4 errors

  val relTheory: List[Clause] = List(
    RuleImpl(FactImpl("son", List(VariableImpl("X"), VariableImpl("Y"))),
      List(FactImpl("father", List(VariableImpl("Y"), VariableImpl("X"))), FactImpl("male", List(VariableImpl("X"))))),
    FactImpl("mul", List(VariableImpl("X"), AtomImpl(0), AtomImpl(0))),
    FactImpl("father", List(AtomImpl("abraham"), AtomImpl("isaac"))),
    FactImpl("father", List(AtomImpl("terach"), AtomImpl("abraham"))),
    FactImpl("male", List(AtomImpl("isaac"))),
    FactImpl("male", List(AtomImpl("abraham"))),
    FactImpl("male", List(AtomImpl("terach"))))

  val relGoals: List[Fact] = List(FactImpl("son", List(VariableImpl("X"), VariableImpl("Y"))))

  val relTheoryNoErrors: List[PzValidation[Clause]] = List(
    Predicate("son")("X", "Y") :- (Predicate("father")("Y", "X"), Predicate("male")("X")),
    Predicate("mul")("X", 0, 0),
    Predicate("father")("abraham", "isaac"),
    Predicate("father")("terach", "abraham"),
    Predicate("male")("isaac"),
    Predicate("male")("abraham"),
    Predicate("male")("terach"))

  val relGoalsNoErrors: List[PzValidation[Fact]] = List(Predicate("son")("X", "Y"))

  val relTheoryWithErrors: List[PzValidation[Clause]] = List(
    Predicate("son")("X", "Y") :- (Predicate("father//")("Y", "X"), Predicate("male")("X")),
    Predicate("mul")("X", 0, 0),
    Predicate("father")("abraham3", "isaac"),
    Predicate("father")("terach", "abraham"),
    Predicate("male_")("isaac"),
    Predicate("male")("abraham"),
    Predicate("male")("terach")) // 3 errors

  val relGoalsWithErrors: List[PzValidation[Fact]] = List(Predicate("Son")("X", "34Y")) // 2 errors

}
