package prologz.resolution

import scalaz.std.function._
import scalaz.std.list._
import scalaz.std.set._
import scalaz.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.functor._
import scalaz.syntax.monoid._
import scala.language.implicitConversions
import prologz.dsl.{Fact, Rule, Struct, Term, Variable}
import prologz.resolution.Substitution.Substitution

/** Implicit helpers for [[prologz.dsl.Term]] and [[prologz.dsl.Clause]] instances */
private[prologz] object Implicits {

  sealed trait RichElement[A] {
    /** Retrieves variables
     *
     *  @return all variables contained in the element
     */
    def getVariables: Set[Variable]
    /** Renames variables in the element
     *
     *  @param variables variables that must be renamed
     *  @return the element after renaming variables
     */
    def rename(variables: Set[Variable]): A
    /** Executes substitution in the element
     *
     *  @param subs substitution that must be performed
     *  @return the element after applying substitution
     */
    def substitute(subs: Substitution): A
  }

  implicit class RichTermList(base: List[Term]) extends RichElement[List[Term]] {
    override def getVariables: Set[Variable] = getVariablesTerms(base)
    override def rename(variables: Set[Variable]): List[Term] = renameTerms(base, variables, variables |+| base.getVariables)
    override def substitute(subs: Substitution): List[Term] = subs.map(substituteTerms).foldLeft(identity[List[Term]](_))((acc, el) => acc map el)(base)
  }

  implicit class RichFact(base: Fact) extends RichElement[Fact] {
    override def getVariables: Set[Variable] = base.args.getVariables
    override def rename(variables: Set[Variable]): Fact = Fact(base.name, base.args.rename(variables))
    override def substitute(subs: Substitution): Fact = Fact(base.name, base.args.substitute(subs))
  }

  implicit class RichFactList(base: List[Fact]) extends RichElement[List[Fact]] {
    override def getVariables: Set[Variable] = base.foldLeft(Set[Variable]())((acc, el) => acc |+| el.getVariables)
    override def rename(variables: Set[Variable]): List[Fact] = base.map(_.rename(variables))
    override def substitute(subs: Substitution): List[Fact] = base.map(_.substitute(subs))
  }

  implicit class RichRule(base: Rule) extends RichElement[Rule] {
    override def getVariables: Set[Variable] = base.head.getVariables |+| base.body.getVariables
    override def rename(variables: Set[Variable]): Rule = Rule(base.head.rename(variables), base.body.rename(variables))
    override def substitute(subs: Substitution): Rule = Rule(base.head.substitute(subs), base.body.substitute(subs))
  }

  /** Retrieves variables
   *
   *  @param terms terms which are considered
   *  @param variables variables found until now
   *  @return all variables contained in terms merged with variables already found
   */
  @scala.annotation.tailrec
  private def getVariablesTerms(terms: List[Term], variables: Set[Variable] = Set()): Set[Variable] = terms match {
    case (v: Variable) :: other => getVariablesTerms(other, variables + v)
    case (s: Struct) :: other => getVariablesTerms(s.args |+| other, variables)
    case _ :: other => getVariablesTerms(other, variables)
    case _ => variables
  }

  /** Renames variables in terms
   *
   *  @param terms terms where variables must be renamed
   *  @param variables variables that must be renamed
   *  @param notValidVars variables that can't be used for renaming
   *  @param attempt index of current rename attempt
   *  @return terms after renaming variables
   */
  private def renameTerms(terms: List[Term], variables: Set[Variable], notValidVars: Set[Variable], attempt: Int = 1): List[Term] = terms match {
    case (v: Variable) :: _ if variables.contains(v) && notValidVars.contains(Variable(v.name + ("'" * attempt))) => renameTerms(terms, variables, notValidVars, attempt + 1)
    case (v: Variable) :: other if variables.contains(v) => Variable(v.name + ("'" * attempt)) :: renameTerms(other, variables, notValidVars)
    case (s: Struct) :: other => Struct(s.name, renameTerms(s.args, variables, notValidVars)) :: renameTerms(other, variables, notValidVars)
    case term :: other => term :: renameTerms(other, variables, notValidVars)
    case _ => Nil
  }

  /** Executes a single substitution in terms
   *
   *  @param sub a variable and a term representing the single substitution
   *  @param terms terms where the single substitution must be performed
   *  @return terms after applying the single substitution
   */
  private def substituteTerms(sub: (Variable, Term))(terms: List[Term]): List[Term] = terms match {
    case (v: Variable) :: other if v.name === sub._1.name => sub._2 :: substituteTerms(sub)(other)
    case (s: Struct) :: other => Struct(s.name, substituteTerms(sub)(s.args)) :: substituteTerms(sub)(other)
    case term :: other => term :: substituteTerms(sub)(other)
    case _ => Nil
  }

}
