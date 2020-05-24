package prologz.utils

import prologz.dsl.{AtomImpl, Fact, FactImpl, Rule, RuleImpl, StructImpl, Term, Variable, VariableImpl}
import prologz.resolution.Substitution
import prologz.resolution.Substitution.Substitution

trait TestUtils {

  val variablesToRename: Set[Variable] = Set(VariableImpl("X"), VariableImpl("Y"),  VariableImpl("Z"))
  val substitution: Substitution = Substitution(VariableImpl("X") -> AtomImpl("x"), VariableImpl("Y") -> AtomImpl("y"))

  val termList: List[Term] = List(VariableImpl("X"), AtomImpl(0), StructImpl("s", List(VariableImpl("Y"), AtomImpl(1))), AtomImpl("a"))
  val termListRenamed: List[Term] = List(VariableImpl("X'"), AtomImpl(0), StructImpl("s", List(VariableImpl("Y'"), AtomImpl(1))), AtomImpl("a"))
  val termListSubstituted: List[Term] = List(AtomImpl("x"), AtomImpl(0), StructImpl("s", List(AtomImpl("y"), AtomImpl(1))), AtomImpl("a"))
  val termListVariables: Set[Variable] = Set(VariableImpl("X"), VariableImpl("Y"))

  val fact: Fact = FactImpl("sampleFact", List(AtomImpl(0), StructImpl("p", List(VariableImpl("X"), VariableImpl("Z")))))
  val factRenamed: Fact = FactImpl("sampleFact", List(AtomImpl(0), StructImpl("p", List(VariableImpl("X'"), VariableImpl("Z'")))))
  val factSubstituted: Fact = FactImpl("sampleFact", List(AtomImpl(0), StructImpl("p", List(AtomImpl("x"), VariableImpl("Z")))))
  val factVariables: Set[Variable] = Set(VariableImpl("X"), VariableImpl("Z"))

  val factList: List[Fact] = List(fact, FactImpl("otherFact", termList))
  val factListRenamed: List[Fact] = List(factRenamed, FactImpl("otherFact", termListRenamed))
  val factListSubstituted: List[Fact] = List(factSubstituted, FactImpl("otherFact", termListSubstituted))
  val factListVariables: Set[Variable] = Set(VariableImpl("X"), VariableImpl("Z"), VariableImpl("Y"))

  val rule: Rule = RuleImpl(FactImpl("ruleHead", List(VariableImpl("Z"), AtomImpl(2.5), AtomImpl("a"), VariableImpl("Y"))), factList)
  val ruleRenamed: Rule = RuleImpl(FactImpl("ruleHead", List(VariableImpl("Z'"), AtomImpl(2.5), AtomImpl("a"), VariableImpl("Y'"))), factListRenamed)
  val ruleSubstituted: Rule = RuleImpl(FactImpl("ruleHead", List(VariableImpl("Z"), AtomImpl(2.5), AtomImpl("a"), AtomImpl("y"))), factListSubstituted)
  val ruleVariables: Set[Variable] = Set(VariableImpl("Z"), VariableImpl("Y"), VariableImpl("X"))

}
