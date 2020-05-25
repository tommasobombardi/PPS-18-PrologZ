package prologz.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.resolution.Implicits.RichFactList
import prologz.resolution.PrologTree.{initializePrologTree, searchPrologTree}
import prologz.utils.PrologSamples

class PrologTreeSpec extends AnyFlatSpec with Matchers with PrologSamples {

  "The prolog tree manager" should "correctly initialize the resolution tree for a program" in {
    val mulInitialTree = initializePrologTree(mulTheory, mulGoals)
    mulInitialTree.isRoot shouldBe true
    mulInitialTree.firstChild shouldBe empty
    mulInitialTree.getLabel shouldBe (mulTheory, mulGoals, Substitution.base(mulGoals.getVariables))
    val relInitialTree = initializePrologTree(relTheory, relGoals)
    relInitialTree.isRoot shouldBe true
    relInitialTree.firstChild shouldBe empty
    relInitialTree.getLabel shouldBe (relTheory, relGoals, Substitution.base(relGoals.getVariables))
  }

  it should "search through the tree retrieving a leaf (valid solution) or the root (end of computation)" in {

  }

}
