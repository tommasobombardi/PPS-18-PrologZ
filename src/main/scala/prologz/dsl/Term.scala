package prologz.dsl

import scalaz.{@@, Tag}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import prologz.resolution.Validation.{InputError, PzValidation}

/** Prolog functor */
sealed trait Functor

/** Prolog term */
sealed trait Term { def toProlog: String }

/** Constant term
 *
 *  @param value value of the constant term
 *  @tparam A type of the constant term
 */
private[prologz] case class Atom[A](value: A) extends Term {
  override def toProlog: String = value.toString
}

/** Variable term
 *
 *  @param name name of the variable term
 */
private[prologz] case class Variable(name: String) extends Term {
  override def toProlog: String = name
}

/** Compound term
 *
 *  @param name name of the compound term
 *  @param args arguments of the compound term
 */
private[prologz] case class Struct(name: String, args: List[Term]) extends Term {
  override def toProlog: String = s"$name(${args.map(_.toProlog).mkString(",")})"
}

/** Factory for [[String @@ Functor]] instances */
object Struct {
  /** Creates a functor
   *
   *  @param name functor name
   *  @return functor if there is no error in name, errors list otherwise
   */
  def apply(name: String): PzValidation[String @@ Functor] = {
    val nameVal1: PzValidation[String] = if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a compound term").failureNel
    val nameVal2: PzValidation[String] = if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError(s"String '$name' is not valid to represent a compound term, because it doesn't contain only letters").failureNel
    val nameVal3: PzValidation[String] = if(name.nonEmpty && name.charAt(0).isLower) name.successNel
      else InputError(s"String '$name' is not valid to represent a compound term, because it doesn't start with a lowercase letter").failureNel
    (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => Tag[String, Functor](name))
  }
}
