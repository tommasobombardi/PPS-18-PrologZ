package prologz.resolution

import scalaz._
import Scalaz._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{AtomImpl, FactImpl, Rule, RuleImpl, StructImpl, Variable, VariableImpl}
import prologz.resolution.Implicits.{RichFact, RichFactList, RichRule, RichTermList}
import prologz.utils.PrologSamples

class ImplicitsSpec extends AnyFlatSpec with Matchers with PrologSamples {

  private val variablesToRename: Set[Variable] = Set(VariableImpl("X"), VariableImpl("Y"),  VariableImpl("Z"))
  private val substitution = Substitution(VariableImpl("X") -> AtomImpl(1), VariableImpl("Y") -> StructImpl("s", List(VariableImpl("Z"))), VariableImpl("Z") -> VariableImpl("Y'"))

  private val relRuleHeadVariables = Set(VariableImpl("X"), VariableImpl("Y"))
  private val relRuleBodyVariables = Set(VariableImpl("Y"), VariableImpl("X"))
  private val mulRuleHeadVariables = Set(VariableImpl("X"), VariableImpl("Y"), VariableImpl("Z"))
  private val mulRuleBodyVariables = Set(VariableImpl("X"), VariableImpl("Y"), VariableImpl("W"), VariableImpl("Z"))

  private val relRuleRenamed = RuleImpl(FactImpl("son", List(VariableImpl("X'"), VariableImpl("Y'"))),
    List(FactImpl("father", List(VariableImpl("Y'"), VariableImpl("X'"))), FactImpl("male", List(VariableImpl("X'")))))
  private val mulRuleRenamed = RuleImpl(FactImpl("mul", List(VariableImpl("X'"), StructImpl("s", List(VariableImpl("Y'"))), VariableImpl("Z'"))),
    List(FactImpl("mul", List(VariableImpl("X'"), VariableImpl("Y'"), VariableImpl("W"))), FactImpl("sum", List(VariableImpl("X'"), VariableImpl("W"), VariableImpl("Z'")))))

  private val relRuleSubstituted = RuleImpl(FactImpl("son", List(AtomImpl(1), StructImpl("s", List(VariableImpl("Y'"))))),
    List(FactImpl("father", List(StructImpl("s", List(VariableImpl("Y'"))), AtomImpl(1))), FactImpl("male", List(AtomImpl(1)))))
  private val mulRuleSubstituted = RuleImpl(FactImpl("mul", List(AtomImpl(1), StructImpl("s", List(StructImpl("s", List(VariableImpl("Y'"))))), VariableImpl("Y'"))),
    List(FactImpl("mul", List(AtomImpl(1), StructImpl("s", List(VariableImpl("Y'"))), VariableImpl("W"))), FactImpl("sum", List(AtomImpl(1), VariableImpl("W"), VariableImpl("Y'")))))

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
