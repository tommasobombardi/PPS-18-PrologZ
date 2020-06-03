package prologz.resolution

import scalaz.Monoid
import scala.language.implicitConversions
import prologz.dsl.{Term, Variable}
import prologz.resolution.Implicits.RichTermList

/** Factory for [[Substitution]] instances
 *  Implicit conversions and helpers for [[Substitution]] instances
 */
private[prologz] object Substitution {

  type Substitution = Map[Variable, Term]

  /** Creates a substitution
   *
   *  @param args tuples containing a variable and a term
   *  @return the corresponding substitution
   */
  def apply(args: (Variable, Term)*): Substitution = args.toMap

  /** Creates an identity substitution
   *
   *  @param variables variables that must be considered in this substitution
   *  @return the identity substitution, where each variable is related to itself
   */
  def base(variables: Set[Variable]): Substitution = variables.zip(variables).toMap

  /** Converts a tuple into a substitution
   *
   * @param arg a tuple containing a variable and a term
   * @return the corresponding substitution
   */
  implicit def fromTuple(arg: (Variable, Term)): Substitution = this(arg)

  /** Policy for substitution accumulation */
  implicit val substitutionMonoid: Monoid[Substitution] = new Monoid[Substitution] {
    override val zero: Substitution = Substitution()
    override def append(s1: Substitution, s2: => Substitution): Substitution = s1.keySet.zip(s1.values.toList.substitute(s2)).toMap
  }

  implicit class RichSubstitution(base: Substitution) {
    def getResult: Substitution = base.filter(sub => sub._1 != sub._2)
    def toProlog: String = s"{${base.map(sub => s"${sub._1.toProlog}/${sub._2.toProlog}").mkString(",")}}"
  }

}
