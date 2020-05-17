package prologz

import prologz.Term.{Term, Variable}
import prologz.Unification._

private[prologz] object Substitution {

  type Substitution = List[(Variable, Term)]

  def apply(args: (Variable, Term)*): Substitution = args.toList

  implicit class RichSubstitution(base: Substitution) {
    def toProlog: String = "{" + base.map(s => s._1.toProlog + "/" + s._2.toProlog).mkString(",") + "}"
    def getResult(variables: Set[Variable]): Substitution = variables.toList.zip(variables.toList.substitute(base)).filter(sub => sub._1 != sub._2)
  }

  implicit def fromTuple(arg: (Variable, Term)): Substitution = this(arg)

}
