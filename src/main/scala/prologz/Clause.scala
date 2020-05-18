package prologz

import scalaz._
import Scalaz._
import prologz.Term.Term
import prologz.Validation.InputError

object Clause {

  sealed trait Clause { def toProlog: String }
  sealed trait Fact extends Clause { def name: String; def args: List[Term] }
  sealed trait Rule extends Clause { def head: Fact; def body: List[Fact] }

  private[prologz] case class FactImpl(override val name: String, override val args: List[Term]) extends Fact {
    override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")."
  }
  private[prologz] case class RuleImpl(override val head: Fact, override val body: List[Fact]) extends Rule {
    override def toProlog: String = head.toProlog.dropRight(1) + ":-" + body.map(_.toProlog.dropRight(1)).mkString(",") + "."
  }

  sealed trait Predicate

  object Predicate {
    def apply(name: String): ValidationNel[String @@ InputError, String @@ Predicate] = {
      val nameVal1: ValidationNel[String @@ InputError, String] =
        if(name.nonEmpty) name.successNel
        else InputError("An empty string is not valid to represent a predicate").failureNel
      val nameVal2: ValidationNel[String @@ InputError, String] =
        if(name.toCharArray.forall(_.isLetter)) name.successNel
        else InputError("String '" + name + "' is not valid to represent a predicate, because it doesn't contain only letters").failureNel
      val nameVal3: ValidationNel[String @@ InputError, String] =
        if(name.nonEmpty && name.charAt(0).isLower) name.successNel
        else InputError("String '" + name + "' is not valid to represent a predicate, because it doesn't start with a lowercase letter").failureNel
      (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => Tag[String, Predicate](name))
    }
  }

  implicit class PredicateRich(base: ValidationNel[String @@ InputError, String @@ Predicate]) {
    def apply(args: ValidationNel[String @@ InputError, Term]*): ValidationNel[String @@ InputError, Fact] = {
      val argsVal: ValidationNel[String @@ InputError, List[Term]] =
        args.foldLeft(List.empty[Term].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
      (base |@| argsVal)((predicate, args) => FactImpl(Tag.unwrap(predicate), args))
    }
  }

  implicit class FactRich(base: ValidationNel[String @@ InputError, Fact]) {
    def :-(facts: ValidationNel[String @@ InputError, Fact]*): ValidationNel[String @@ InputError, Clause] = setBody(facts:_*)
    def setBody(facts: ValidationNel[String @@ InputError, Fact]*): ValidationNel[String @@ InputError, Clause] = {
      val factsVal: ValidationNel[String @@ InputError, List[Fact]] =
        if(facts.nonEmpty) facts.foldLeft(List.empty[Fact].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
        else InputError("Body (namely the list of facts) of a rule must be not empty").failureNel
      (base |@| factsVal)((head, body) => RuleImpl(head, body))
    }
  }

  implicit def fromFact(fact: ValidationNel[String @@ InputError, Fact]): ValidationNel[String @@ InputError, Clause] =
    fact.asInstanceOf[ValidationNel[String @@ InputError, Clause]]

}
