package prologz.core

/** General prolog term */
sealed trait Term {
  /**
   *  @return corresponding prolog representation
   */
  def toProlog: String
}

/** Constant prolog term
 *
 *  @tparam A type of the constant prolog term
 */
sealed trait Atom[A] extends Term { def value: A }

private[core] case class AtomImpl[A](override val value: A) extends Atom[A] {
  override def toProlog: String = value.toString
}

/** Functor useful to create a compound prolog term */
sealed trait PzFunctor

/** Compound prolog term */
sealed trait Struct extends Term { def name: String; def args: List[Term] }

private[core] case class StructImpl(override val name: String, override val args: List[Term]) extends Struct {
  override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")"
}

/** Variable prolog term */
sealed trait Variable extends Term { def name: String }

private[core] case class VariableImpl(override val name: String) extends Variable {
  override def toProlog: String = name
}
