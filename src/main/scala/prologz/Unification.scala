package prologz

import scalaz._
import Scalaz._
import prologz.Clause.{Clause, Fact, FactImpl, Rule, RuleImpl}
import prologz.Substitution._
import prologz.Term.{Struct, StructImpl, Term, Variable, VariableImpl}

private[prologz] object Unification {

  implicit class RichTermList(base: List[Term]) {
    def getVariables: Set[Variable] = getVariablesTerms(base)
    def rename(variables: Set[Variable]): List[Term] = renameTerms(base, variables, variables |+| base.getVariables)
    def substitute(subs: Substitution): List[Term] = subs.map(substituteTerms).foldLeft(identity[List[Term]](_))((acc, el) => acc >>> el)(base)
  }

  implicit class RichFact(base: Fact) {
    def getVariables: Set[Variable] = base.args.getVariables
    def rename(variables: Set[Variable]): Fact = FactImpl(base.name, base.args.rename(variables))
    def substitute(subs: Substitution): Fact = FactImpl(base.name, base.args.substitute(subs))
    def unify(goal: Fact, otherGoals: List[Fact]) : Option[(Substitution, List[Fact])] = {
      if(base.name != goal.name || base.args.size != goal.args.size) None else
        ((fact: Fact) => for {
          renamedFact <- StateT[Option, List[Fact], Fact](goals => (goals, fact.rename(goals.getVariables)).some)
          subs <- StateT[Option, List[Fact], Substitution](goals => unifyTerms(renamedFact.args, goals.head.args).map((goals.tail, _)))
          _ <- StateT[Option, List[Fact], Unit](goals => (goals.substitute(subs), {}).some)
        } yield subs)(base)(goal :: otherGoals).map(_.swap)
    }
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
    def unify(goal: Fact, otherGoals: List[Fact]) : Option[(Substitution, List[Fact])] = {
      if(base.head.name != goal.name || base.head.args.size != goal.args.size) None else
        ((rule: Rule) => for {
          renamedRule <- StateT[Option, List[Fact], Rule](goals => (goals, rule.rename(goals.getVariables)).some)
          subs <- StateT[Option, List[Fact], Substitution](goals => unifyTerms(renamedRule.head.args, goals.head.args).map((goals.tail, _)))
          _ <- StateT[Option, List[Fact], Unit](goals => (renamedRule.body.rename(goals.getVariables.diff(renamedRule.head.getVariables)) |+| goals, {}).some)
          _ <- StateT[Option, List[Fact], Unit](goals => (goals.substitute(subs), {}).some)
        } yield subs)(base)(goal :: otherGoals).map(_.swap)
    }
  }

  implicit class RichClause(clause: Clause) {
    def unify(goal: Fact, otherGoals: List[Fact]) : Option[(Substitution, List[Fact])] = clause match {
      case fact: Fact => fact.unify(goal, otherGoals)
      case rule: Rule => rule.unify(goal, otherGoals)
    }
  }

  @scala.annotation.tailrec
  private def unifyTerms(factTerms: List[Term], goalTerms: List[Term], subs: Substitution = Substitution()) : Option[Substitution] = (factTerms, goalTerms) match {
    case (f :: ft, g :: gt) if f == g => unifyTerms(ft, gt, subs)
    case ((v: Variable) :: _, (s: Struct) :: _) if s.args.getVariables.contains(v) => None // avoid endless loop (e.g. X can't be assigned to s(X))
    case ((s: Struct) :: _, (v: Variable) :: _) if s.args.getVariables.contains(v) => None // avoid endless loop (e.g. X can't be assigned to s(X))
    case ((s1: Struct) :: ft, (s2: Struct) :: gt) if s1.name == s2.name && s1.args.size == s2.args.size => unifyTerms(s1.args |+| ft, s2.args |+| gt, subs)
    case ((v1: Variable) :: ft, (v2: Variable) :: gt) => unifyTerms(ft.substitute(v2, v1), gt.substitute(v2, v1), subs |+| (v2, v1))
    case ((v: Variable) :: ft, (t: Term) :: gt) => unifyTerms(ft.substitute(v, t), gt.substitute(v, t), subs |+| (v, t))
    case ((t: Term) :: ft, (v: Variable) :: gt) => unifyTerms(ft.substitute(v, t), gt.substitute(v, t), subs |+| (v, t))
    case (_ :: _, _) => None // theory and goal terms don't unify because they have different length
    case (_, _ :: _) => None // theory and goal terms don't unify because they have different length
    case _ => Option(subs) // successfully unified
  }

  @scala.annotation.tailrec
  private def getVariablesTerms(terms: List[Term], variables: Set[Variable] = Set()): Set[Variable] = terms match {
    case (v: Variable) :: other => getVariablesTerms(other, variables + v)
    case (s: Struct) :: other => getVariablesTerms(s.args |+| other, variables)
    case _ :: other => getVariablesTerms(other, variables)
    case _ => variables
  }

  private def renameTerms(terms: List[Term], variables: Set[Variable], notValidVars: Set[Variable], attempt: Int = 1): List[Term] = terms match {
    case (v: Variable) :: _ if variables.contains(v) && notValidVars.contains(VariableImpl(v.name + ("a" * attempt))) => renameTerms(terms, variables, notValidVars, attempt + 1)
    case (v: Variable) :: other if variables.contains(v) => VariableImpl(v.name + ("a" * attempt)) :: renameTerms(other, variables, notValidVars)
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
