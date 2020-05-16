package prologz

import prologz.Term._

private[prologz] object Substitution {

  type Substitution = List[(Variable, Term)]

  def apply(args: (Variable, Term)*): Substitution = args.toList

  implicit class RichSubstitution(base: Substitution) {
    def toProlog: String = "{" + base.map(s => s._1.toProlog + "/" + s._2.toProlog).mkString(",") + "}"
  }

  implicit def fromTuple(arg: (Variable, Term)): Substitution = this(arg)

}