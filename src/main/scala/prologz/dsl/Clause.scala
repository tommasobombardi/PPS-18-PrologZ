package prologz.dsl

import scalaz.{@@, Tag}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import prologz.resolution.Validation.{InputError, PzValidation}

/** Prolog predicate */
sealed trait Predicate

/** Prolog clause */
sealed trait Clause { def toProlog: String }

/** Fact clause
 *
 *  @param name name of the fact
 *  @param args arguments of the fact
 */
private[prologz] case class Fact(name: String, args: List[Term]) extends Clause {
  override def toProlog: String = s"$name(${args.map(_.toProlog).mkString(",")})."
}

/** Rule clause
 *
 * @param head fact which is the head of the rule
 * @param body facts which are the body of the rule
 */
private[prologz] case class Rule(head: Fact, body: List[Fact]) extends Clause {
  override def toProlog: String = s"${head.toProlog.dropRight(1)}:-${body.map(_.toProlog.dropRight(1)).mkString(",")}."
}

/** Factory for [[String @@ Predicate]] instances */
object Predicate {
  /** Creates a predicate
   *
   *  @param name predicate name
   *  @return predicate if there is no error in name, errors list otherwise
   */
  def apply(name: String): PzValidation[String @@ Predicate] = {
    val nameVal1: PzValidation[String] = if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a predicate").failureNel
    val nameVal2: PzValidation[String] = if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError(s"String '$name' is not valid to represent a predicate, because it doesn't contain only letters").failureNel
    val nameVal3: PzValidation[String] = if(name.nonEmpty && name.charAt(0).isLower) name.successNel
      else InputError(s"String '$name' is not valid to represent a predicate, because it doesn't start with a lowercase letter").failureNel
    (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => Tag[String, Predicate](name))
  }
}
