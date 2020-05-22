package prologz.core

import scalaz._
import Scalaz._
import prologz.core.Validation.{InputError, PzValidation}

/** Prolog term */
sealed trait Term { def toProlog: String }

/** Constant term
 *
 *  @tparam A type of the constant term
 */
sealed trait Atom[A] extends Term { def value: A }

private[core] case class AtomImpl[A](override val value: A) extends Atom[A] {
  override def toProlog: String = value.toString
}

/** Variable term */
sealed trait Variable extends Term { def name: String }

private[core] case class VariableImpl(override val name: String) extends Variable {
  override def toProlog: String = name
}

/** Functor useful to create a compound term */
sealed trait PzFunctor

/** Compound term */
sealed trait Struct extends Term { def name: String; def args: List[Term] }

private[core] case class StructImpl(override val name: String, override val args: List[Term]) extends Struct {
  override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")"
}

/** Factory for [[String @@ prologz.core.PzFunctor]] instances */
object Struct {
  /** Creates a functor
   *
   *  @param name functor name
   *  @return functor if there are no errors in name, errors list otherwise
   */
  def apply(name: String): PzValidation[String @@ PzFunctor] = {
    val nameVal1: PzValidation[String] =
      if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a compound term").failureNel
    val nameVal2: PzValidation[String] =
      if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError("String '" + name + "' is not valid to represent a compound term, because it doesn't contain only letters").failureNel
    val nameVal3: PzValidation[String] =
      if(name.nonEmpty && name.charAt(0).isLower) name.successNel
      else InputError("String '" + name + "' is not valid to represent a compound term, because it doesn't start with a lowercase letter").failureNel
    (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => Tag[String, PzFunctor](name))
  }
}