package prologz

import scalaz._
import Scalaz._
import prologz.Term._

object Unification {

  type Substitution = List[(Variable, Term)]

  object Substitution {
    def apply(args: (Variable, Term)*): Substitution = args.toList
  }

  implicit class RichSubstitution(base: Substitution) {
    def toProlog: String = "{" + base.map(s => s._1.toProlog + "/" + s._2.toProlog).mkString(",") + "}"
  }

}
