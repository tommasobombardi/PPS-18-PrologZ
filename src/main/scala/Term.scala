import scalaz._
import Scalaz._

object Term {

  sealed trait Term { def toProlog: String }
  sealed trait Atom extends Term { def name: String }
  sealed trait Double extends Term { def value: scala.Double }
  sealed trait Int extends Term { def value: scala.Int }
  sealed trait Struct extends Term { def name: String; def args: List[Term] }
  sealed trait Variable extends Term { def name: String }

  private case class AtomImpl(override val name: String) extends Atom {
    override def toProlog: String = name
  }
  private case class DoubleImpl(override val value: scala.Double) extends Double {
    override def toProlog: String = value.toString
  }
  private case class IntImpl(override val value: scala.Int) extends Int {
    override def toProlog: String = value.toString
  }
  private case class StructImpl(override val name: String, override val args: List[Term]) extends Struct {
    override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")"
  }
  private case class VariableImpl(override val name: String) extends Variable {
    override def toProlog: String = name
  }

  object Struct {
    def apply(name: String)(args: ValidationNel[IllegalArgumentException, Term]*): ValidationNel[IllegalArgumentException, Struct] = {
      val nameVal1: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty) name.successNel
        else new IllegalArgumentException("An empty string is not valid to represent a compound term").failureNel
      val nameVal2: ValidationNel[IllegalArgumentException, String] =
        if(name.toCharArray.forall(_.isLetter)) name.successNel
        else new IllegalArgumentException("String '" + name + "' is not valid to represent a compound term, because it doesn't contain only letters").failureNel
      val nameVal3: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty && name.charAt(0).isLower) name.successNel
        else new IllegalArgumentException("String '" + name + "' is not valid to represent a compound term, because it doesn't start with a lowercase letter").failureNel
      val argsVal: ValidationNel[IllegalArgumentException, List[Term]] =
        if(args.nonEmpty) args.foldLeft(List.empty[Term].successNel[IllegalArgumentException])((accumulator, element) => (accumulator |@| element)((acc, el) => el :: acc))
        else new IllegalArgumentException("Argument list of compound term '" + name + "' must be not empty").failureNel
      (nameVal1 |@| nameVal2 |@| nameVal3 |@| argsVal)((name, _, _, args) => StructImpl(name, args))
    }
  }

  implicit def fromString(name: String): ValidationNel[IllegalArgumentException, Term] = {
    val nameVal1: ValidationNel[IllegalArgumentException, String] =
      if(name.nonEmpty) name.successNel
      else new IllegalArgumentException("An empty string is not valid to represent a term").failureNel
    val nameVal2: ValidationNel[IllegalArgumentException, String] =
      if(name.toCharArray.forall(_.isLetter)) name.successNel
      else new IllegalArgumentException("String '" + name + "' is not valid to represent a term, because it doesn't contain only letters").failureNel
    (nameVal1 |@| nameVal2)((name, _) => if(name.charAt(0).isLower) AtomImpl(name) else VariableImpl(name))
  }

  implicit def fromInt(value: scala.Int): ValidationNel[IllegalArgumentException, Term] = IntImpl(value).asInstanceOf[Term].successNel

  implicit def fromDouble(value: scala.Double): ValidationNel[IllegalArgumentException, Term] = DoubleImpl(value).asInstanceOf[Term].successNel

}
