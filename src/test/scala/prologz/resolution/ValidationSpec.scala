package prologz.resolution

import scalaz._
import Scalaz._
import org.scalatest.flatspec.AnyFlatSpec
import prologz.dsl.{Clause, Fact}
import prologz.resolution.Validation.{InputError, PzValidation, validateProgram}
import prologz.utils.Utils

class ValidationSpec extends AnyFlatSpec with Utils {

  "The validate method" should "transform a valid program in its validated version" in {
    val mulValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(mulTheoryNoErrors, mulGoalsNoErrors)
    assert(mulValidation.isSuccess && mulValidation.getOrElse((Nil, Nil)) == (mulTheory, mulGoals))
    val relValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(relTheoryNoErrors, relGoalsNoErrors)
    assert(relValidation.isSuccess && relValidation.getOrElse((Nil, Nil)) == (relTheory, relGoals))
  }

  it should "detect if there are errors only in program theory and report them" in {
    val mulValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(mulTheoryWithErrors, mulGoalsNoErrors)
    assert(mulValidation.isFailure && mulValidation.swap.getOrElse(InputError("").wrapNel).size == 7) // 7 (7 + 0)
    val relValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(relTheoryWithErrors, relGoalsNoErrors)
    assert(relValidation.isFailure && relValidation.swap.getOrElse(InputError("").wrapNel).size == 3) // 3 (3 + 0)
  }

  it should "detect if there are errors only in program goals and report them" in {
    val mulValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(mulTheoryNoErrors, mulGoalsWithErrors)
    assert(mulValidation.isFailure && mulValidation.swap.getOrElse(InputError("").wrapNel).size == 4) // 4 (0 + 4)
    val relValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(relTheoryNoErrors, relGoalsWithErrors)
    assert(relValidation.isFailure && relValidation.swap.getOrElse(InputError("").wrapNel).size == 2) // 2 (0 + 2)
  }

  it should "detect if there are errors both in program theory and goals and report them" in {
    val mulValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(mulTheoryWithErrors, mulGoalsWithErrors)
    assert(mulValidation.isFailure && mulValidation.swap.getOrElse(InputError("").wrapNel).size == 11) // 11 (7 + 4)
    val relValidation: PzValidation[(List[Clause], List[Fact])] = validateProgram(relTheoryWithErrors, relGoalsWithErrors)
    assert(relValidation.isFailure && relValidation.swap.getOrElse(InputError("").wrapNel).size == 5) // 5 (3 + 2)
  }

}
