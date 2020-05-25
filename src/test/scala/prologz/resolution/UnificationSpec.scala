package prologz.resolution

import scalaz._
import Scalaz._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.{Fact, Rule}
import prologz.resolution.Unification.RichClause
import prologz.utils.Utils

class UnificationSpec extends AnyFlatSpec with Matchers with Utils {

  private val mulTheoryFacts: List[Fact] = mulTheory.flatMap{ case fact: Fact => fact.some; case _ => None }
  private val mulTheoryRules: List[Rule] = mulTheory.flatMap{ case rule: Rule => rule.some; case _ => None }
  private val relTheoryFacts: List[Fact] = relTheory.flatMap{ case fact: Fact => fact.some; case _ => None }
  private val relTheoryRules: List[Rule] = relTheory.flatMap{ case rule: Rule => rule.some; case _ => None }

  "A theory fact" should "not unify to a goal with a different predicate name" in {
    for(theory <- mulTheoryFacts; goal <- mulGoals if theory.name != goal.name) theory.unify(goal, Nil) shouldBe None
    for(theory <- relTheoryFacts; goal <- relGoals if theory.name != goal.name) theory.unify(goal, Nil) shouldBe None
  }

  it should "not unify to a goal with a different arguments arity" in {
    for(theory <- mulTheoryFacts; goal <- mulGoals if theory.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe None
    for(theory <- relTheoryFacts; goal <- relGoals if theory.args.size != goal.args.size) theory.unify(goal, Nil) shouldBe None
  }

  it should "unify to a compatible goal retrieving the substitution and other goals after its application" in {

  }


}
