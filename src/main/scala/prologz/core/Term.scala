package prologz.core

/** General representation of a prolog term */
sealed trait Term {
  /**
   *  @return corresponding prolog term
   */
  def toProlog: String
}

/** Representation of a constant prolog term
 *
 *  @tparam A type of the constant prolog term
 */
sealed trait Atom[A] extends Term {
  /**
   *  @return value of the constant prolog term
   */
  def value: A
}

/** Representation of a compound prolog term */
sealed trait Struct extends Term {
  /**
   *  @return functor name of the compound prolog term
   */
  def name: String
  /**
   *  @return list of terms of the compound prolog term
   */
  def args: List[Term]
}

/** Representation of a variable prolog term */
sealed trait Variable extends Term {
  /**
   *  @return name of the variable prolog term
   */
  def name: String
}

private[core] case class AtomImpl[A](override val value: A) extends Atom[A] {
  override def toProlog: String = value.toString
}

private[core] case class StructImpl(override val name: String, override val args: List[Term]) extends Struct {
  override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")"
}

private[core] case class VariableImpl(override val name: String) extends Variable {
  override def toProlog: String = name
}

sealed trait Compound
