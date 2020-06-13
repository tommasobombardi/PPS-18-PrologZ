package prologz.resolution

import scalaz.std.set._
import scalaz.syntax.monoid._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{Atom, Fact, Rule, Struct, Variable}
import prologz.resolution.Implicits.{RichFact, RichFactList, RichRule, RichTermList}
import prologz.utils.PrologSamples

class ImplicitsSpec extends AnyFlatSpec with Matchers with PrologSamples {

  private val variablesToRename: Set[Variable] = Set(Variable("X"), Variable("Y"),  Variable("Z"))
  private val substitution = Substitution(Variable("X") -> Atom(1), Variable("Y") -> Struct("s", List(Variable("Z"))), Variable("Z") -> Variable("Y'"))

  private val mulRuleHeadVariables = Set(Variable("X"), Variable("Y"), Variable("Z"))
  private val mulRuleBodyVariables = Set(Variable("X"), Variable("Y"), Variable("W"), Variable("Z"))
  private val mulRuleRenamed = Rule(Fact("mul", List(Variable("X'"), Struct("s", List(Variable("Y'"))), Variable("Z'"))),
    List(Fact("mul", List(Variable("X'"), Variable("Y'"), Variable("W"))), Fact("sum", List(Variable("X'"), Variable("W"), Variable("Z'")))))
  private val mulRuleSubstituted = Rule(Fact("mul", List(Atom(1), Struct("s", List(Struct("s", List(Variable("Y'"))))), Variable("Y'"))),
    List(Fact("mul", List(Atom(1), Struct("s", List(Variable("Y'"))), Variable("W"))), Fact("sum", List(Atom(1), Variable("W"), Variable("Y'")))))

  private val relRuleHeadVariables = Set(Variable("X"), Variable("Y"))
  private val relRuleBodyVariables = Set(Variable("Y"), Variable("X"))
  private val relRuleRenamed = Rule(Fact("son", List(Variable("X'"), Variable("Y'"))),
    List(Fact("father", List(Variable("Y'"), Variable("X'"))), Fact("male", List(Variable("X'")))))
  private val relRuleSubstituted = Rule(Fact("son", List(Atom(1), Struct("s", List(Variable("Y'"))))),
    List(Fact("father", List(Struct("s", List(Variable("Y'"))), Atom(1))), Fact("male", List(Atom(1)))))

  "A term list" should "retrieve all the variables it contains" in {
    mulTheory(3).asInstanceOf[Rule].head.args.getVariables shouldBe mulRuleHeadVariables
    relTheory.head.asInstanceOf[Rule].head.args.getVariables shouldBe relRuleHeadVariables
  }

  it should "perform a renaming generating a correctly updated term list" in {
    mulTheory(3).asInstanceOf[Rule].head.args.rename(variablesToRename) shouldBe mulRuleRenamed.head.args
    relTheory.head.asInstanceOf[Rule].head.args.rename(variablesToRename) shouldBe relRuleRenamed.head.args
  }

  it should "perform a substitution generating a correctly updated term list" in {
    mulTheory(3).asInstanceOf[Rule].head.args.substitute(substitution) shouldBe mulRuleSubstituted.head.args
    relTheory.head.asInstanceOf[Rule].head.args.substitute(substitution) shouldBe relRuleSubstituted.head.args
  }

  "A fact" should "retrieve all the variables it contains" in {
    mulTheory(3).asInstanceOf[Rule].head.getVariables shouldBe mulRuleHeadVariables
    relTheory.head.asInstanceOf[Rule].head.getVariables shouldBe relRuleHeadVariables
  }

  it should "perform a renaming generating a correctly updated fact" in {
    mulTheory(3).asInstanceOf[Rule].head.rename(variablesToRename) shouldBe mulRuleRenamed.head
    relTheory.head.asInstanceOf[Rule].head.rename(variablesToRename) shouldBe relRuleRenamed.head
  }

  it should "perform a substitution generating a correctly updated fact" in {
    mulTheory(3).asInstanceOf[Rule].head.substitute(substitution) shouldBe mulRuleSubstituted.head
    relTheory.head.asInstanceOf[Rule].head.substitute(substitution) shouldBe relRuleSubstituted.head
  }

  "A fact list" should "retrieve all the variables it contains" in {
    mulTheory(3).asInstanceOf[Rule].body.getVariables shouldBe mulRuleBodyVariables
    relTheory.head.asInstanceOf[Rule].body.getVariables shouldBe relRuleBodyVariables
  }

  it should "perform a renaming generating a correctly updated fact list" in {
    mulTheory(3).asInstanceOf[Rule].body.rename(variablesToRename) shouldBe mulRuleRenamed.body
    relTheory.head.asInstanceOf[Rule].body.rename(variablesToRename) shouldBe relRuleRenamed.body
  }

  it should "perform a substitution generating a correctly updated fact list" in {
    mulTheory(3).asInstanceOf[Rule].body.substitute(substitution) shouldBe mulRuleSubstituted.body
    relTheory.head.asInstanceOf[Rule].body.substitute(substitution) shouldBe relRuleSubstituted.body
  }

  "A rule" should "retrieve all the variables it contains" in {
    mulTheory(3).asInstanceOf[Rule].getVariables shouldBe mulRuleHeadVariables |+| mulRuleBodyVariables
    relTheory.head.asInstanceOf[Rule].getVariables shouldBe relRuleHeadVariables |+| relRuleBodyVariables
  }

  it should "perform a renaming generating a correctly updated rule" in {
    mulTheory(3).asInstanceOf[Rule].rename(variablesToRename) shouldBe mulRuleRenamed
    relTheory.head.asInstanceOf[Rule].rename(variablesToRename) shouldBe relRuleRenamed
  }

  it should "perform a substitution generating a correctly updated rule" in {
    mulTheory(3).asInstanceOf[Rule].substitute(substitution) shouldBe mulRuleSubstituted
    relTheory.head.asInstanceOf[Rule].substitute(substitution) shouldBe relRuleSubstituted
  }

}
