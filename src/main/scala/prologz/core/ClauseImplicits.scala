package prologz.core

import scalaz._
import Scalaz._
import scala.language.implicitConversions
import prologz.core.Validation.{InputError, PzValidation}

object ClauseImplicits {

  object Predicate {
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

  implicit class PredicateRich(base: PzValidation[String @@ Predicate]) {
    def apply(args: PzValidation[Term]*): PzValidation[Fact] = {
      val argsVal: PzValidation[List[Term]] =
        args.foldLeft(List.empty[Term].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
      (base |@| argsVal)((predicate, args) => FactImpl(Tag.unwrap(predicate), args))
    }
  }

  implicit class FactRich(base: PzValidation[Fact]) {
    def :-(facts: PzValidation[Fact]*): PzValidation[Clause] = setBody(facts:_*)
    def setBody(facts: PzValidation[Fact]*): PzValidation[Clause] = {
      val factsVal: PzValidation[List[Fact]] =
        if(facts.nonEmpty) facts.foldLeft(List.empty[Fact].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
        else InputError("Body (namely the list of facts) of a rule must be not empty").failureNel
      (base |@| factsVal)((head, body) => RuleImpl(head, body))
    }
  }

  implicit def fromFact(fact: PzValidation[Fact]): PzValidation[Clause] = fact.asInstanceOf[PzValidation[Clause]]

}
