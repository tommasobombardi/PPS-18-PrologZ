package prologz.resolution

import scalaz._
import Scalaz._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{AtomImpl, Fact, FactImpl, Rule, StructImpl, VariableImpl}
import prologz.resolution.Unification.RichClause
import prologz.utils.Utils

class UnificationSpec extends AnyFlatSpec with Matchers with Utils {

  private val mulTheoryFacts = mulTheory.flatMap{ case fact: Fact => fact.some; case _ => None }
  private val mulTheoryRules = mulTheory.flatMap{ case rule: Rule => rule.some; case _ => None }
  private val relTheoryFacts = relTheory.flatMap{ case fact: Fact => fact.some; case _ => None }
  private val relTheoryRules = relTheory.flatMap{ case rule: Rule => rule.some; case _ => None }

  private val otherMulGoal = FactImpl("mul", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), AtomImpl(0), VariableImpl("Y")))
  private val otherMulGoalRes = Substitution(VariableImpl("Y") -> AtomImpl(0))
  private val mulGoalAfterOther = FactImpl("mul", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), AtomImpl(0)))
  private val mulGoalRes = Substitution(VariableImpl("Y") -> StructImpl("s", List(StructImpl("s", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))))))))
  private val otherMulGoalAfterFirst = FactImpl("mul", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0))))), AtomImpl(0), StructImpl("s", List(StructImpl("s", List(StructImpl("s", List(AtomImpl(0)))))))))

  private val otherRelGoal = FactImpl("father", List(VariableImpl("X"), VariableImpl("Y")))
  private val otherRelGoalRes = Substitution(VariableImpl("X") -> AtomImpl("abraham"), VariableImpl("Y") -> AtomImpl("isaac"))
  private val relGoalAfterOther = FactImpl("son", List(VariableImpl("abraham"), VariableImpl("isaac")))
  private val relGoalRes = Substitution(VariableImpl("X") -> AtomImpl("isaac"), VariableImpl("Y") -> AtomImpl("abraham"))
  private val otherRelGoalAfterFirst = FactImpl("father", List(VariableImpl("isaac"), VariableImpl("abraham")))

  "A theory fact" should "not unify to a goal with a different predicate name" in {
    for(theory <- mulTheoryFacts; goal <- mulGoals if theory.name != goal.name) theory.unify(goal, Nil) shouldBe None
    for(theory <- relTheoryFacts; goal <- relGoals if theory.name != goal.name) theory.unify(goal, Nil) shouldBe None
  }

  it should "not unify to a goal with a different arguments arity" in {
    for(theory <- mulTheoryFacts; goal <- mulGoals if theory.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe None
    for(theory <- relTheoryFacts; goal <- relGoals if theory.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe None
  }

  it should "unify to a compatible goal retrieving the substitution and other goals after its application" in {
    mulTheoryFacts


  }


}
