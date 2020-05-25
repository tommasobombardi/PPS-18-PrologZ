package prologz.resolution

import scalaz._
import Scalaz._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{AtomImpl, StructImpl, VariableImpl}
import prologz.resolution.Substitution.{Substitution, substitutionMonoid}

class SubstitutionSpec extends AnyFlatSpec with Matchers {

  private val substitution1: Substitution = Map(VariableImpl("X") -> VariableImpl("X"), VariableImpl("Y") -> VariableImpl("Y"), VariableImpl("Z") -> VariableImpl("Z"))
  private val substitution2: Substitution = Map(VariableImpl("X") -> AtomImpl(1), VariableImpl("Y") -> StructImpl("s", List(VariableImpl("Z"))), VariableImpl("Z") -> VariableImpl("Y'"))
  private val substitution3: Substitution = Map(VariableImpl("Y'") -> AtomImpl(3), VariableImpl("Z") -> VariableImpl("X"))

  private val expected132 = Substitution(VariableImpl("X") -> AtomImpl(1), VariableImpl("Y") -> StructImpl("s", List(VariableImpl("Y'"))), VariableImpl("Z") -> AtomImpl(1))
  private val expected23 = Substitution(VariableImpl("X") -> AtomImpl(1), VariableImpl("Y") -> StructImpl("s", List(AtomImpl(3))), VariableImpl("Z") -> AtomImpl(3)) // 123, 213, 231
  private val expected32 = Substitution(VariableImpl("Y'") -> AtomImpl(3), VariableImpl("Z") -> AtomImpl(1)) // 312, 321

  "The substitution factory" should "create a valid substitution starting from tuples containing a variable and a term" in {
    Substitution((VariableImpl("X"), VariableImpl("X")), (VariableImpl("Y"), VariableImpl("Y")), (VariableImpl("Z"), VariableImpl("Z"))) shouldBe substitution1
    Substitution((VariableImpl("X"), AtomImpl(1)), (VariableImpl("Y"), StructImpl("s", List(VariableImpl("Z")))), (VariableImpl("Z"), VariableImpl("Y'"))) shouldBe substitution2
    Substitution((VariableImpl("Y'"), AtomImpl(3)), (VariableImpl("Z"), VariableImpl("X"))) shouldBe substitution3
  }

  it should "create a valid identity substitution starting from a set of variables" in {
    Substitution.base(Set(VariableImpl("X"), VariableImpl("Y"), VariableImpl("Z"))) shouldBe substitution1
  }

  "A valid substitution" should "be appended to other valid substitutions generating a valid composition" in {
    substitution1 |+| substitution2 |+| substitution3 shouldBe expected23 // expected123 == expected23
    substitution1 |+| substitution3 |+| substitution2 shouldBe expected132
    substitution2 |+| substitution1 |+| substitution3 shouldBe expected23 // expected213 == expected23
    substitution2 |+| substitution3 |+| substitution1 shouldBe expected23 // expected231 == expected23
    substitution3 |+| substitution1 |+| substitution2 shouldBe expected32 // expected312 == expected32
    substitution3 |+| substitution2 |+| substitution1 shouldBe expected32 // expected321 == expected32
  }

}
