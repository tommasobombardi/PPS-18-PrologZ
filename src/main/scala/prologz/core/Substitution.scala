package prologz.core

import scalaz._
import Scalaz._
import scala.language.implicitConversions
import prologz.core.PrologImplicits._

private[core] object Substitution {

  type Substitution = Map[Variable, Term]

  def apply(args: (Variable, Term)*): Substitution = args.toMap
  def base(variables: Set[Variable]): Substitution = variables.zip(variables).toMap

  implicit def fromTuple(arg: (Variable, Term)): Substitution = this(arg)

  implicit val substitutionMonoid: Monoid[Substitution] = new Monoid[Substitution] {
    override val zero: Substitution = Substitution()
    override def append(s1: Substitution, s2: => Substitution): Substitution = s1.keySet.zip(s1.values.toList.substitute(s2)).toMap
  }

  implicit class RichSubstitution(base: Substitution) {
    def getResult: Substitution = base.filter(sub => sub._1 != sub._2)
    def toProlog: String = "{" + base.map(s => s._1.toProlog + "/" + s._2.toProlog).mkString(",") + "}"
  }



}
