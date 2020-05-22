package prologz.core

sealed trait Clause { def toProlog: String }
sealed trait Fact extends Clause { def name: String; def args: List[Term] }
sealed trait Rule extends Clause { def head: Fact; def body: List[Fact] }

private[core] case class FactImpl(override val name: String, override val args: List[Term]) extends Fact {
  override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")."
}
private[core] case class RuleImpl(override val head: Fact, override val body: List[Fact]) extends Rule {
  override def toProlog: String = head.toProlog.dropRight(1) + ":-" + body.map(_.toProlog.dropRight(1)).mkString(",") + "."
}

sealed trait Predicate