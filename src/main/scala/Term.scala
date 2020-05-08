import scalaz._
import Scalaz._

object Term {

  sealed trait Term { def toProlog: String }
  sealed trait Atom extends Term { def name: String }
  sealed trait Int extends Term { def value: scala.Int }
  sealed trait Struct extends Term { def name: String; def args: List[Term] }
  sealed trait Variable extends Term { def name: String }

  private case class AtomImpl(override val name: String) extends Atom {
    override def toProlog: String = name
  }

  private case class IntImpl(override val value: scala.Int) extends Int {
    override def toProlog: String = value.toString
  }

  private case class StructImpl(override val name: String, override val args: List[Term]) extends Struct {
    override def toProlog: String = name + "(" + args.map(t => t.toProlog).mkString(",") + ")"
  }

  private case class VariableImpl(override val name: String) extends Variable {
    override def toProlog: String = name
  }

  object Struct {
    def apply(name: String)(args: Term*): ValidationNel[IllegalArgumentException, Struct] = {
      val nameVal1: ValidationNel[IllegalArgumentException, String] =
        if(name.isEmpty) new IllegalArgumentException("String representing compound term must be not empty").failureNel else name.successNel
      val nameVal2: ValidationNel[IllegalArgumentException, String] =
        if(name.exists(c => !c.isLetter)) new IllegalArgumentException("String representing compound term must contain only letter").failureNel else name.successNel
      val nameVal3: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty && name.charAt(0).isUpper) new IllegalArgumentException("String representing compound term must start with a lowercase letter").failureNel else name.successNel
      val argsVal: ValidationNel[IllegalArgumentException, Seq[Term]] =
        if(args.isEmpty) new IllegalArgumentException("Argument list in compound term must be not empty").failureNel else args.successNel
      (nameVal1 |@| nameVal2 |@| nameVal3 |@| argsVal)((name, _, _, args) => StructImpl(name, args.toList))
    }
  }

  implicit def fromString(name: String): Term = {
    require(name.nonEmpty, "String representing term must be not empty")
    require(name.forall(c => c.isLetter), "String representing term must contain only letters")
    if(name.charAt(0).isLower) AtomImpl(name) else VariableImpl(name)
  }

  implicit def fromInt(value: scala.Int): Term = IntImpl(value)

}
