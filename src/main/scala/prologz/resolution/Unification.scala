package prologz.resolution

import scalaz.StateT
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.monoid._
import scalaz.syntax.std.option._
import prologz.dsl.{Clause, Fact, Rule, Struct, Term, Variable}
import prologz.dsl.TermImplicits.TermEqual
import prologz.resolution.Implicits.{RichFact, RichFactList, RichRule, RichTermList}
import prologz.resolution.Substitution.{Substitution, fromTuple}

/** Implicit helpers for [[prologz.dsl.Clause]] instances */
private[prologz] object Unification {

  implicit class RichClause(base: Clause) {
    /** Unifies a theory clause (the instance of this class) with a goal
     *
     *  @param goal goal which is considered
     *  @param otherGoals other goals that must be solved
     *  @return resulting substitution and other goals after its application if clause unifies with goal, none otherwise
     */
    def unify(goal: Fact, otherGoals: List[Fact]) : Option[(Substitution, List[Fact])] = base match {
      case fact: Fact => unifyFact(fact, goal, otherGoals)
      case rule: Rule => unifyRule(rule, goal, otherGoals)
    }
  }

  private def unifyFact(fact: Fact, goal: Fact, otherGoals: List[Fact]) : Option[(Substitution, List[Fact])] = {
    if(fact.name =/= goal.name || fact.args.size =/= goal.args.size) none[(Substitution, List[Fact])] else
      ((fact: Fact) => for {
        renamedFact <- StateT[Option, List[Fact], Fact](goals => (goals, fact.rename(goals.getVariables)).some)
        subs <- StateT[Option, List[Fact], Substitution](goals => unifyTerms(renamedFact.args, goals.head.args).map((goals.tail, _)))
        _ <- StateT[Option, List[Fact], Unit](goals => (goals.substitute(subs), {}).some)
      } yield subs)(fact)(goal :: otherGoals).map(_.swap)
  }

  private def unifyRule(rule: Rule, goal: Fact, otherGoals: List[Fact]) : Option[(Substitution, List[Fact])] = {
    if(rule.head.name =/= goal.name || rule.head.args.size =/= goal.args.size) none[(Substitution, List[Fact])] else
      ((rule: Rule) => for {
        renamedRule <- StateT[Option, List[Fact], Rule](goals => (goals, rule.rename(goals.getVariables)).some)
        subs <- StateT[Option, List[Fact], Substitution](goals => unifyTerms(renamedRule.head.args, goals.head.args).map((goals.tail, _)))
        _ <- StateT[Option, List[Fact], Unit](goals => (renamedRule.body.rename(goals.getVariables.diff(renamedRule.head.getVariables)) |+| goals, {}).some)
        _ <- StateT[Option, List[Fact], Unit](goals => (goals.substitute(subs), {}).some)
      } yield subs)(rule)(goal :: otherGoals).map(_.swap)
  }

  /** Unifies two terms lists (the first from a theory clause and the second from a goal)
   *
   *  @param factTerms terms list contained in theory clause
   *  @param goalTerms terms list contained in goal
   *  @param subs substitution that has been performed until now
   *  @return resulting substitution if the terms lists unify, none otherwise
   */
  @scala.annotation.tailrec
  private def unifyTerms(factTerms: List[Term], goalTerms: List[Term], subs: Substitution = Substitution()) : Option[Substitution] = (factTerms, goalTerms) match {
    case (f :: ft, g :: gt) if f === g => unifyTerms(ft, gt, subs)
    case ((v: Variable) :: _, (s: Struct) :: _) if s.args.getVariables.contains(v) => none[Substitution] // avoid endless loop (e.g. X can't be assigned to s(X))
    case ((s: Struct) :: _, (v: Variable) :: _) if s.args.getVariables.contains(v) => none[Substitution] // avoid endless loop (e.g. X can't be assigned to s(X))
    case ((s1: Struct) :: ft, (s2: Struct) :: gt) if s1.name === s2.name && s1.args.size === s2.args.size => unifyTerms(s1.args |+| ft, s2.args |+| gt, subs)
    case ((v1: Variable) :: ft, (v2: Variable) :: gt) => unifyTerms(ft.substitute(v2, v1), gt.substitute(v2, v1), subs + ((v2, v1)) )
    case ((v: Variable) :: ft, (t: Term) :: gt) => unifyTerms(ft.substitute(v, t), gt.substitute(v, t), subs + ((v, t)))
    case ((t: Term) :: ft, (v: Variable) :: gt) => unifyTerms(ft.substitute(v, t), gt.substitute(v, t), subs + ((v, t)))
    case (_ :: _, _) => none[Substitution] // theory and goal terms don't unify because they have different length
    case (_, _ :: _) => none[Substitution] // theory and goal terms don't unify because they have different length
    case _ => subs.some // successfully unified
  }

}
