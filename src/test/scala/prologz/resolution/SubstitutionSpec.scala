package prologz.resolution

import scalaz.syntax.monoid._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{Atom, Struct, Variable}
import prologz.resolution.Substitution.{RichSubstitution, Substitution, SubstitutionMonoid}

class SubstitutionSpec extends AnyFlatSpec with Matchers {

  private val substitution1: Substitution = Map(Variable("X") -> Variable("X"), Variable("Y") -> Variable("Y"), Variable("Z") -> Variable("Z"))
  private val substitution2: Substitution = Map(Variable("X") -> Atom(1), Variable("Y") -> Struct("s", List(Variable("Z"))), Variable("Z") -> Variable("Y'"))
  private val substitution3: Substitution = Map(Variable("Y'") -> Atom(3), Variable("Z") -> Variable("X"))

  private val expected123 = Substitution(Variable("X") -> Atom(1), Variable("Y") -> Struct("s", List(Atom(3))), Variable("Z") -> Atom(3))
  private val expected132 = Substitution(Variable("X") -> Atom(1), Variable("Y") -> Struct("s", List(Variable("Y'"))), Variable("Z") -> Atom(1))
  private val expected312 = Substitution(Variable("Y'") -> Atom(3), Variable("Z") -> Atom(1))
  private val expected321 = Substitution(Variable("Y'") -> Atom(3), Variable("Z") -> Atom(1))

  "The substitution factory" should "create a valid substitution starting from tuples containing a variable and a term" in {
    Substitution((Variable("X"), Variable("X")), (Variable("Y"), Variable("Y")), (Variable("Z"), Variable("Z"))) shouldBe substitution1
    Substitution((Variable("X"), Atom(1)), (Variable("Y"), Struct("s", List(Variable("Z")))), (Variable("Z"), Variable("Y'"))) shouldBe substitution2
    Substitution((Variable("Y'"), Atom(3)), (Variable("Z"), Variable("X"))) shouldBe substitution3
  }

  it should "create a valid identity substitution starting from a set of variables" in {
    Substitution.base(Set(Variable("X"), Variable("Y"), Variable("Z"))) shouldBe substitution1
  }

  "A valid substitution" should "be appended to other valid substitutions generating the proper composition" in {
    substitution1 |+| substitution2 |+| substitution3 shouldBe expected123
    substitution1 |+| substitution3 |+| substitution2 shouldBe expected132
    substitution3 |+| substitution1 |+| substitution2 shouldBe expected312
    substitution3 |+| substitution2 |+| substitution1 shouldBe expected321
  }

  it should "retrieve an equivalent substitution without identity associations" in {
    substitution1.getResult should have size 0
    substitution2.getResult shouldBe substitution2
    substitution3.getResult shouldBe substitution3
  }

}
