package prologz

import scalaz._
import Scalaz._
import prologz.Term._

object Clause {

  sealed trait Predicate { def name: String }
  sealed trait Clause { def toProlog: String }
  sealed trait Fact extends Clause { def name: String; def args: List[Term] }
  sealed trait Rule extends Clause { def head: Fact; def body: List[Fact] }

  private[prolog] case class PredicateImpl(override val name: String) extends Predicate
  private[prolog] case class FactImpl(override val name: String, override val args: List[Term]) extends Fact {
    override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")."
  }
  private[prolog] case class RuleImpl(override val head: Fact, override val body: List[Fact]) extends Rule {
    override def toProlog: String = head.toProlog.dropRight(1) + ":-" + body.map(_.toProlog.dropRight(1)).mkString(",") + "."
  }

  object Predicate {
    def apply(name: String): ValidationNel[IllegalArgumentException, Predicate] = {
      val nameVal1: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty) name.successNel
        else new IllegalArgumentException("An empty string is not valid to represent a predicate").failureNel
      val nameVal2: ValidationNel[IllegalArgumentException, String] =
        if(name.toCharArray.forall(_.isLetter)) name.successNel
        else new IllegalArgumentException("String '" + name + "' is not valid to represent a predicate, because it doesn't contain only letters").failureNel
      val nameVal3: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty && name.charAt(0).isLower) name.successNel
        else new IllegalArgumentException("String '" + name + "' is not valid to represent a predicate, because it doesn't start with a lowercase letter").failureNel
      (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => PredicateImpl(name))
    }
  }

  implicit class PredicateRich(base: ValidationNel[IllegalArgumentException, Predicate]) {
    def apply(args: ValidationNel[IllegalArgumentException, Term]*): ValidationNel[IllegalArgumentException, Fact] = {
      val argsVal: ValidationNel[IllegalArgumentException, List[Term]] =
        args.foldLeft(List.empty[Term].successNel[IllegalArgumentException])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
      (base |@| argsVal)((predicate, args) => FactImpl(predicate.name, args))
    }
  }

  implicit class FactRich(base: ValidationNel[IllegalArgumentException, Fact]) {
    def :-(facts: ValidationNel[IllegalArgumentException, Fact]*): ValidationNel[IllegalArgumentException, Clause] = setBody(facts:_*)
    def setBody(facts: ValidationNel[IllegalArgumentException, Fact]*): ValidationNel[IllegalArgumentException, Clause] = {
      val factsVal: ValidationNel[IllegalArgumentException, List[Fact]] =
        if(facts.nonEmpty) facts.foldLeft(List.empty[Fact].successNel[IllegalArgumentException])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
        else new IllegalArgumentException("Body (namely the list of facts) of a rule must be not empty").failureNel
      (base |@| factsVal)((head, body) => RuleImpl(head, body))
    }
  }

  implicit def fromFact(fact: ValidationNel[IllegalArgumentException, Fact]): ValidationNel[IllegalArgumentException, Clause] =
    fact.asInstanceOf[ValidationNel[IllegalArgumentException, Clause]]

}
