package prologz.resolution

import scalaz.std.option._
import scalaz.syntax.std.option._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{AtomImpl, Fact, FactImpl, Rule, StructImpl, VariableImpl}
import prologz.resolution.Unification.RichClause
import prologz.utils.PrologSamples

class UnificationSpec extends AnyFlatSpec with Matchers with PrologSamples {

  private val mulTheoryFacts = mulTheory.flatMap{ case fact: Fact => fact.some; case _ => none[Fact] }
  private val mulTheoryRules = mulTheory.flatMap{ case rule: Rule => rule.some; case _ => none[Rule] }

  private val relTheoryFacts = relTheory.flatMap{ case fact: Fact => fact.some; case _ => none[Fact] }
  private val relTheoryRules = relTheory.flatMap{ case rule: Rule => rule.some; case _ => none[Rule] }

  private val otherMulGoal = FactImpl("mul", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), AtomImpl(0), VariableImpl("Y")))
  private val otherMulGoalRes = Substitution(VariableImpl("X") -> StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), VariableImpl("Y") -> AtomImpl(0))
  private val mulGoalRes = Substitution(VariableImpl("X") -> StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))),
    VariableImpl("Y'") -> StructImpl("s", List(AtomImpl(0))), VariableImpl("Y") -> VariableImpl("Z"))

  private val otherRelGoal = FactImpl("father", List(VariableImpl("X"), VariableImpl("Y")))
  private val otherRelGoalRes = Substitution(VariableImpl("X") -> AtomImpl("abraham"), VariableImpl("Y") -> AtomImpl("isaac"))
  private val relGoalRes = Substitution(VariableImpl("X") -> VariableImpl("X'"), VariableImpl("Y") -> VariableImpl("Y'"))

  "A theory fact" should "not unify with a goal having a different predicate name" in {
    for(theory <- mulTheoryFacts; goal <- mulGoals if theory.name != goal.name) theory.unify(goal, Nil) shouldBe empty
    for(theory <- relTheoryFacts; goal <- relGoals if theory.name != goal.name) theory.unify(goal, Nil) shouldBe empty
  }

  it should "not unify with a goal having a different arguments arity" in {
    for(theory <- mulTheoryFacts; goal <- mulGoals if theory.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe empty
    for(theory <- relTheoryFacts; goal <- relGoals if theory.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe empty
  }

  it should "unify with a goal having the same predicate name and all arguments that unify, retrieving the proper substitution" in {
    mulTheoryFacts(1).unify(otherMulGoal, Nil) shouldBe (otherMulGoalRes, Nil).some
    relTheoryFacts.head.unify(otherRelGoal, Nil) shouldBe (otherRelGoalRes, Nil).some
  }

  "A theory rule" should "not unify with a goal having a predicate name different from its head" in {
    for(theory <- mulTheoryRules; goal <- mulGoals if theory.head.name != goal.name) theory.unify(goal, Nil) shouldBe empty
    for(theory <- relTheoryRules; goal <- relGoals if theory.head.name != goal.name) theory.unify(goal, Nil) shouldBe empty
  }

  it should "not unify with a goal having an arguments arity different from its head" in {
    for(theory <- mulTheoryRules; goal <- mulGoals if theory.head.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe empty
    for(theory <- relTheoryRules; goal <- relGoals if theory.head.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe empty
  }

  it should "unify with a goal having the same predicate name of its head and all arguments that unify, retrieving the proper substitution" in {
    mulTheoryRules(1).unify(mulGoals.head, Nil).map(_._1) shouldBe mulGoalRes.some
    relTheoryRules.head.unify(relGoals.head, Nil).map(_._1) shouldBe relGoalRes.some
  }

}
