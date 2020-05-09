import scalaz._
import Scalaz._
import Term.Term

object Clause {

  sealed trait Clause { def toProlog: String }
  sealed trait Fact extends Clause { def name: String; def args: List[Term]; def :-(facts: Fact*): Rule }
  sealed trait Rule extends Clause { def head: Fact; def body: List[Fact] }

  sealed trait Predicate { def name: String; def apply(args: Term*): Fact }

  private case class FactImpl(override val name: String, override val args: List[Term]) extends Fact {
    override def :-(facts: Fact*): Rule = {
      require(facts.nonEmpty, "Body of a rule must be not empty")
      RuleImpl(this, facts.toList)
    }
    override def toProlog: String = name + "(" + args.map(t => t.toProlog).mkString(",") + ")."
  }

  private case class RuleImpl(override val head: Fact, override val body: List[Fact]) extends Rule {
    override def toProlog: String = head.toProlog.dropRight(1) + ":-" + body.map(f => f.toProlog.dropRight(1)).mkString(",") + "."
  }

  private case class PredicateImpl(override val name: String) extends Predicate {
    override def apply(args: Term*): Fact = FactImpl(name, args.toList)
  }

  object Predicate {
    def apply(name: String): ValidationNel[IllegalArgumentException, Predicate] = {
      val nameVal1: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty) name.successNel
        else new IllegalArgumentException("An empty string is not valid to represent a predicate").failureNel
      val nameVal2: ValidationNel[IllegalArgumentException, String] =
        if(name.forall(_.isLetter)) name.successNel
        else new IllegalArgumentException("String '" + name + "' is not valid to represent a predicate, because it doesn't contain only letters").failureNel
      val nameVal3: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty && name.charAt(0).isLower) name.successNel
        else new IllegalArgumentException("String '" + name + "' is not valid to represent a predicate, because it doesn't start with a lowercase letter").failureNel
      (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => PredicateImpl(name))
    }
  }

}
