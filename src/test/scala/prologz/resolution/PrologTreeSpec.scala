package prologz.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.resolution.Implicits.RichFactList
import prologz.resolution.PrologTree.{initializePrologTree, searchPrologTree}
import prologz.utils.PrologSamples

class PrologTreeSpec extends AnyFlatSpec with Matchers with PrologSamples {

  "The prolog tree manager" should "correctly initialize the resolution tree for a program" in {
    val mulTreeInitial = initializePrologTree(mulTheory, mulGoals)
    mulTreeInitial shouldBe Symbol("isRoot")
    mulTreeInitial.firstChild shouldBe empty
    mulTreeInitial.getLabel shouldBe (mulTheory, mulGoals, Substitution.base(mulGoals.getVariables))
    val relTreeInitial = initializePrologTree(relTheory, relGoals)
    relTreeInitial shouldBe Symbol("isRoot")
    relTreeInitial.firstChild shouldBe empty
    relTreeInitial.getLabel shouldBe (relTheory, relGoals, Substitution.base(relGoals.getVariables))
  }

  it should "search through the tree retrieving a leaf (valid solution) or the root (end of computation)" in {
    // this test executes, for each prolog program, three searching steps navigating the prolog tree
    // if the computation ends before the third step, the same node (root) it's retrieved several times
    var mulTree = initializePrologTree(mulTheory, mulGoals)
    var relTree = initializePrologTree(relTheory, relGoals)
    for (_ <- 1 to 3) yield {
      mulTree = searchPrologTree(mulTheory, mulTree)
      relTree = searchPrologTree(mulTheory, relTree)
      mulTree.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))
      relTree.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))
    }
  }

}
