package prologz.dsl

import scalaz.{@@, Tag}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import prologz.resolution.Validation.{InputError, PzValidation}

/** Prolog term */
sealed trait Term { def toProlog: String }

/** Constant term
 *
 *  @tparam A type of the constant term
 */
sealed trait Atom[A] extends Term { def value: A }

private[prologz] case class AtomImpl[A](override val value: A) extends Atom[A] {
  override def toProlog: String = value.toString
}

/** Variable term */
sealed trait Variable extends Term { def name: String }

private[prologz] case class VariableImpl(override val name: String) extends Variable {
  override def toProlog: String = name
}

/** Prolog functor */
sealed trait Functor

/** Compound term */
sealed trait Struct extends Term { def name: String; def args: List[Term] }

private[prologz] case class StructImpl(override val name: String, override val args: List[Term]) extends Struct {
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
    val nameVal1: PzValidation[String] =
      if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a compound term").failureNel
    val nameVal2: PzValidation[String] =
      if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError(s"String $name is not valid to represent a compound term, because it doesn't contain only letters").failureNel
    val nameVal3: PzValidation[String] =
      if(name.nonEmpty && name.charAt(0).isLower) name.successNel
      else InputError(s"String $name is not valid to represent a compound term, because it doesn't start with a lowercase letter").failureNel
    (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => Tag[String, Functor](name))
  }
}
