package prologz.core

import scalaz._
import Scalaz._
import scala.language.implicitConversions
import prologz.core.Substitution.Substitution

private[core] object PrologImplicits {

  implicit class RichTermList(base: List[Term]) {
    def getVariables: Set[Variable] = getVariablesTerms(base)
    def rename(variables: Set[Variable]): List[Term] = renameTerms(base, variables, variables |+| base.getVariables)
    def substitute(subs: Substitution): List[Term] = subs.map(substituteTerms).foldLeft(identity[List[Term]](_))((acc, el) => acc >>> el)(base)
  }

  implicit class RichFact(base: Fact) {
    def getVariables: Set[Variable] = base.args.getVariables
    def rename(variables: Set[Variable]): Fact = FactImpl(base.name, base.args.rename(variables))
    def substitute(subs: Substitution): Fact = FactImpl(base.name, base.args.substitute(subs))
  }

  implicit class RichFactList(base: List[Fact]) {
    def getVariables: Set[Variable] = base.foldLeft(Set[Variable]())((acc, el) => acc |+| el.getVariables)
    def rename(variables: Set[Variable]): List[Fact] = base.map(_.rename(variables))
    def substitute(subs: Substitution): List[Fact] = base.map(_.substitute(subs))
  }

  implicit class RichRule(base: Rule) {
    def getVariables: Set[Variable] = base.head.getVariables |+| base.body.getVariables
    def rename(variables: Set[Variable]): Rule = RuleImpl(base.head.rename(variables), base.body.map(_.rename(variables)))
    def substitute(subs: Substitution): Rule = RuleImpl(base.head.substitute(subs), base.body.map(_.substitute(subs)))
  }

  @scala.annotation.tailrec
  private def getVariablesTerms(terms: List[Term], variables: Set[Variable] = Set()): Set[Variable] = terms match {
    case (v: Variable) :: other => getVariablesTerms(other, variables + v)
    case (s: Struct) :: other => getVariablesTerms(s.args |+| other, variables)
    case _ :: other => getVariablesTerms(other, variables)
    case _ => variables
  }

  private def renameTerms(terms: List[Term], variables: Set[Variable], notValidVars: Set[Variable], attempt: Int = 1): List[Term] = terms match {
    case (v: Variable) :: _ if variables.contains(v) && notValidVars.contains(VariableImpl(v.name + ("'" * attempt))) => renameTerms(terms, variables, notValidVars, attempt + 1)
    case (v: Variable) :: other if variables.contains(v) => VariableImpl(v.name + ("'" * attempt)) :: renameTerms(other, variables, notValidVars)
    case (s: Struct) :: other => StructImpl(s.name, renameTerms(s.args, variables, variables |+| s.args.getVariables)) :: renameTerms(other, variables, notValidVars)
    case term :: other => term :: renameTerms(other, variables, notValidVars)
    case _ => Nil
  }

  private def substituteTerms(sub: (Variable, Term))(terms: List[Term]): List[Term] = terms match {
    case (v: Variable) :: other if v == sub._1 => sub._2 :: substituteTerms(sub)(other)
    case (s: Struct) :: other => StructImpl(s.name, substituteTerms(sub)(s.args)) :: substituteTerms(sub)(other)
    case term :: other => term :: substituteTerms(sub)(other)
    case _ => Nil
  }

}