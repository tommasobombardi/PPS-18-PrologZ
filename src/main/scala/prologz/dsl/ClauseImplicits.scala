package prologz.dsl

import scalaz.{@@, Tag}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scala.language.implicitConversions
import prologz.resolution.InputError
import prologz.resolution.Validation.{InputError, PzValidation}

/** Implicit conversions and helpers for [[prologz.dsl.Clause]] instances */
object ClauseImplicits {

  implicit class PredicateRich(base: PzValidation[String @@ Predicate]) {
    /** Creates a fact, the instance of this class is the predicate
     *
     *  @param args terms, which still need to be validated
     *  @return the fact if there is no error in predicate or args, errors list otherwise
     */
    def apply(args: PzValidation[Term]*): PzValidation[Fact] = {
      val argsVal: PzValidation[List[Term]] =
        args.foldLeft(List.empty[Term].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
      (base |@| argsVal)((predicate, args) => FactImpl(Tag.unwrap(predicate), args))
    }
  }

  implicit class FactRich(base: PzValidation[Fact]) {
    /** Creates a rule, the instance of this class is the head of the rule
     *
     *  @param facts body of the rule, which still need to be validated
     *  @return the rule if there is no error in head or body, errors list otherwise
     */
    def :-(facts: PzValidation[Fact]*): PzValidation[Rule] = setBody(facts:_*)
    def setBody(facts: PzValidation[Fact]*): PzValidation[Rule] = {
      val factsVal: PzValidation[List[Fact]] =
        if(facts.nonEmpty) facts.foldLeft(List.empty[Fact].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
        else InputError("Body (namely the list of facts) of a rule must be not empty").failureNel
      (base |@| factsVal)((head, body) => RuleImpl(head, body))
    }
  }

  implicit def fromFact(fact: PzValidation[Fact]): PzValidation[Clause] = fact.asInstanceOf[PzValidation[Clause]]

  implicit def fromRule(rule: PzValidation[Rule]): PzValidation[Clause] = rule.asInstanceOf[PzValidation[Clause]]

}
