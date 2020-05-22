package prologz.core

import scalaz._
import Scalaz._
import scala.language.implicitConversions
import prologz.core.Validation.{InputError, PzValidation}

object ClauseImplicits {



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
