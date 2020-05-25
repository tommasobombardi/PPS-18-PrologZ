package prologz.resolution

import scalaz._
import Scalaz._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{Predicate, Struct}
import prologz.dsl.ClauseImplicits.{FactRich, PredicateRich}
import prologz.dsl.TermImplicits.{FunctorRich, fromInt, fromString}
import prologz.resolution.Validation.{InputError, validateProgram}
import utils.PrologSamples

class ValidationSpec extends AnyFlatSpec with Matchers with PrologSamples {

  private val mulGoalsNoErrors = List(Predicate("mul")(Struct("s")(Struct("s")(0)), Struct("s")(Struct("s")(0)), "Y"))
  private val mulTheoryNoErrors = List(Predicate("sum")("X", 0, "X"), Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")) :- Predicate("sum")("X", "Y", "Z"),
    Predicate("mul")("X", 0, 0), Predicate("mul")("X", Struct("s")("Y"), "Z") :- (Predicate("mul")("X", "Y", "W"), Predicate("sum")("X", "W", "Z")))

  private val relGoalsNoErrors = List(Predicate("son")("X", "Y"))
  private val relTheoryNoErrors = List(Predicate("son")("X", "Y") :- (Predicate("father")("Y", "X"), Predicate("male")("X")),
    Predicate("father")("abraham", "isaac"), Predicate("father")("terach", "abraham"), Predicate("male")("isaac"), Predicate("male")("abraham"), Predicate("male")("terach"))

  private val mulGoalsWithErrors = List(Predicate("Mul")(Struct("s")(Struct("s1")(0)), Struct("S")(Struct("s")(0)), "Y_")) // 4 errors
  private val mulTheoryWithErrors = List(Predicate("Sum")("X4", 0, "X"), Predicate("sum")("X", Struct("Ss")("Y"), Struct("s")("Z++")) :- Predicate("sum")("X", "Y.", "Z"),
    Predicate("mul")("X", 0, 0), Predicate("mul")("X", Struct("s")(":Y"), "Z") :- (Predicate("Mul")("X", "Y", "W"), Predicate("sum")("Xè", "W", "Z"))) // 7 errors

  private val relGoalsWithErrors = List(Predicate("Son")("X", "34Y")) // 2 errors
  private val relTheoryWithErrors = List(Predicate("son")("X", "Y") :- (Predicate("father//")("Y", "X"), Predicate("male")("X")), Predicate("father")("abraham3", "isaac"),
    Predicate("father")("terach", "abraham"), Predicate("male_")("isaac"), Predicate("male")("abraham"), Predicate("male")("terach")) // 3 errors

  "Applying validation to a program" should "transform a valid program in its validated version" in {
    validateProgram(mulTheoryNoErrors, mulGoalsNoErrors)getOrElse((Nil, Nil)) shouldBe (mulTheory, mulGoals)
    validateProgram(relTheoryNoErrors, relGoalsNoErrors).getOrElse((Nil, Nil)) shouldBe (relTheory, relGoals)
  }

  it should "detect if there are errors only in program theory and report them" in {
    validateProgram(mulTheoryWithErrors, mulGoalsNoErrors).swap.getOrElse(InputError("").wrapNel) should have size 7 // 7 (7 + 0)
    validateProgram(relTheoryWithErrors, relGoalsNoErrors).swap.getOrElse(InputError("").wrapNel) should have size 3 // 3 (3 + 0)
  }

  it should "detect if there are errors only in program goals and report them" in {
    validateProgram(mulTheoryNoErrors, mulGoalsWithErrors).swap.getOrElse(InputError("").wrapNel) should have size 4 // 4 (0 + 4)
    validateProgram(relTheoryNoErrors, relGoalsWithErrors).swap.getOrElse(InputError("").wrapNel) should have size 2 // 2 (0 + 2)
  }

  it should "detect if there are errors both in program theory and goals and report them" in {
    validateProgram(mulTheoryWithErrors, mulGoalsWithErrors).swap.getOrElse(InputError("").wrapNel) should have size 11 // 11 (7 + 4)
    validateProgram(relTheoryWithErrors, relGoalsWithErrors).swap.getOrElse(InputError("").wrapNel) should have size 5 // 5 (3 + 2)
  }

}
