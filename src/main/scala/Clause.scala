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
    def apply(name: String): Predicate = {
      require(name.nonEmpty, "String representing predicate must be not empty")
      require(name.forall(c => c.isLetter), "String representing predicate must contain only letters")
      require(name.charAt(0).isLower, "String representing predicate must start with a lowercase letter")
      PredicateImpl(name)
    }
  }

}
