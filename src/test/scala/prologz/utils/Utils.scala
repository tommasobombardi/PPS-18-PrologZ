package prologz.utils

import prologz.dsl.{Clause, Fact, Predicate, Struct}
import prologz.dsl.ClauseImplicits._
import prologz.dsl.TermImplicits._
import prologz.resolution.Validation.PzValidation

trait Utils {

  val mulTheory: List[PzValidation[Clause]] = List(
    Predicate("sum")("X", 0, "X"),
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")) :- Predicate("sum")("X", "Y", "Z"),
    Predicate("mul")("X", 0, 0),
    Predicate("mul")("X", Struct("s")("Y"), "Z") :- (Predicate("mul")("X", "Y", "W"), Predicate("sum")("X", "W", "Z")))

  val mulGoals: List[PzValidation[Fact]] = List(Predicate("mul")(Struct("s")(Struct("s")(0)), Struct("s")(Struct("s")(0)), "Y"))

  val relTheory: List[PzValidation[Clause]] = List(
    Predicate("son")("X", "Y") :- (Predicate("father")("Y", "X"), Predicate("male")("X")),
    Predicate("mul")("X", 0, 0),
    Predicate("father")("abraham", "isaac"),
    Predicate("father")("terach", "abraham"),
    Predicate("male")("isaac"),
    Predicate("male")("abraham"),
    Predicate("male")("terach"))

  val relGoals: List[PzValidation[Fact]] = List(Predicate("son")("X", "Y"))


}
