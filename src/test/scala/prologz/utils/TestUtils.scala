package prologz.utils

import prologz.dsl.{AtomImpl, Fact, FactImpl, StructImpl, Term, Variable, VariableImpl}
import prologz.resolution.Substitution
import prologz.resolution.Substitution.Substitution

trait TestUtils {

  val emptyTermList: List[Term] = Nil
  val emptyFactList: List[Fact] = Nil

  // In Prolog, the following substitution is represented as {X/x,X'/xx,Y/y,Y'/yy}
  val substitution: Substitution = Substitution(VariableImpl("X") -> AtomImpl("x"), VariableImpl("X'") -> AtomImpl("xx"), VariableImpl("Y") -> AtomImpl("y"), VariableImpl("Y'") -> AtomImpl("yy"))
  val variablesToRename: Set[Variable] = Set(VariableImpl("X"), VariableImpl("Y"), VariableImpl("X'"), VariableImpl("Y'"))

  val termList: List[Term] = List(VariableImpl("X"), AtomImpl(0), StructImpl("s", List(VariableImpl("X'"), AtomImpl(1))), AtomImpl("a"))
  val termListRenamed: List[Term] = List(VariableImpl("X''"), AtomImpl(0), StructImpl("s", List(VariableImpl("X'"), AtomImpl(1))), AtomImpl("a"))
  val termListSubstituted: List[Term] = List(AtomImpl("x"), AtomImpl(0), StructImpl("s", List(AtomImpl("xx"), AtomImpl(1))), AtomImpl("a"))




}
