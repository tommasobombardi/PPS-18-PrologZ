package prologz.resolution

import scalaz._
import Scalaz._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.resolution.Validation.{InputError, validateProgram}
import prologz.utils.Utils

class ValidationSpec extends AnyFlatSpec with Matchers with Utils {

  "The validate method" should "transform a valid program in its validated version" in {
    validateProgram(mulTheoryNoErrors, mulGoalsNoErrors)getOrElse((Nil, Nil)) shouldBe (mulTheory, mulGoals)
    validateProgram(relTheoryNoErrors, relGoalsNoErrors).getOrElse((Nil, Nil)) shouldBe (relTheory, relGoals)
  }

  it should "detect if there are errors only in program theory and report them" in {
    validateProgram(mulTheoryWithErrors, mulGoalsNoErrors).swap.getOrElse(InputError("").wrapNel).size shouldBe 7 // 7 (7 + 0)
    validateProgram(relTheoryWithErrors, relGoalsNoErrors).swap.getOrElse(InputError("").wrapNel).size shouldBe 3 // 3 (3 + 0)
  }

  it should "detect if there are errors only in program goals and report them" in {
    validateProgram(mulTheoryNoErrors, mulGoalsWithErrors).swap.getOrElse(InputError("").wrapNel).size shouldBe 4 // 4 (0 + 4)
    validateProgram(relTheoryNoErrors, relGoalsWithErrors).swap.getOrElse(InputError("").wrapNel).size shouldBe 2 // 2 (0 + 2)
  }

  it should "detect if there are errors both in program theory and goals and report them" in {
    validateProgram(mulTheoryWithErrors, mulGoalsWithErrors).swap.getOrElse(InputError("").wrapNel).size shouldBe 11 // 11 (7 + 4)
    validateProgram(relTheoryWithErrors, relGoalsWithErrors).swap.getOrElse(InputError("").wrapNel).size shouldBe 5 // 5 (3 + 2)
  }

}
