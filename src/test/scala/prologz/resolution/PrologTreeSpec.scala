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
    val mulTreeInitial = initializePrologTree(mulTheory, mulGoals)
    val mulTreeStep1 = searchPrologTree(mulTheory, mulTreeInitial)
    mulTreeStep1.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))
    val mulTreeStep2 = searchPrologTree(mulTheory, mulTreeStep1)
    mulTreeStep2.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))
    val mulTreeStep3 = searchPrologTree(mulTheory, mulTreeStep2)
    mulTreeStep3.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))

    val relTreeInitial = initializePrologTree(relTheory, relGoals)
    val relTreeStep1 = searchPrologTree(mulTheory, relTreeInitial)
    relTreeStep1.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))
    val relTreeStep2 = searchPrologTree(mulTheory, relTreeStep1)
    relTreeStep2.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))
    val relTreeStep3 = searchPrologTree(mulTheory, relTreeStep2)
    relTreeStep3.map(_._2) should (be a Symbol("isRoot") or have(Symbol("getLabel")(Nil)))
  }

}
