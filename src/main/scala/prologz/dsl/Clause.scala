package prologz.dsl

import scalaz._
import Scalaz._
import prologz.resolution.Validation.{InputError, PzValidation}

/** Prolog clause */
sealed trait Clause { def toProlog: String }

/** Prolog predicate */
sealed trait Predicate

/** Fact clause */
sealed trait Fact extends Clause { def name: String; def args: List[Term] }

private[prologz] case class FactImpl(override val name: String, override val args: List[Term]) extends Fact {
  override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")."
}

/** Rule clause */
sealed trait Rule extends Clause { def head: Fact; def body: List[Fact] }

private[prologz] case class RuleImpl(override val head: Fact, override val body: List[Fact]) extends Rule {
  override def toProlog: String = head.toProlog.dropRight(1) + ":-" + body.map(_.toProlog.dropRight(1)).mkString(",") + "."
}

/** Factory for [[String @@ Predicate]] instances */
object Predicate {
  /** Creates a predicate
   *
   *  @param name predicate name
   *  @return predicate if there is no error in name, errors list otherwise
   */
  def apply(name: String): PzValidation[String @@ Predicate] = {
    val nameVal1: PzValidation[String] =
      if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a predicate").failureNel
    val nameVal2: PzValidation[String] =
      if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError("String '" + name + "' is not valid to represent a predicate, because it doesn't contain only letters").failureNel
    val nameVal3: PzValidation[String] =
      if(name.nonEmpty && name.charAt(0).isLower) name.successNel
      else InputError("String '" + name + "' is not valid to represent a predicate, because it doesn't start with a lowercase letter").failureNel
    (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => Tag[String, Predicate](name))
  }
}
