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
    override def toProlog: String = name + "(" + args.map(_.toProlog).mkString(",") + ")"
  }
  private case class VariableImpl(override val name: String) extends Variable {
    override def toProlog: String = name
  }

  object Struct {
    //noinspection FindEmptyCheck
    def apply(name: String)(args: ValidationNel[IllegalArgumentException, Term]*): ValidationNel[IllegalArgumentException, Struct] = {
      val nameVal1: ValidationNel[IllegalArgumentException, String] =
        if(name.isEmpty) new IllegalArgumentException("String representing compound term must be not empty").failureNel else name.successNel
      val nameVal2: ValidationNel[IllegalArgumentException, String] =
        if(name.find(!_.isLetter).isDefined) new IllegalArgumentException("String representing compound term must contain only letter").failureNel else name.successNel
      val nameVal3: ValidationNel[IllegalArgumentException, String] =
        if(name.nonEmpty && name.charAt(0).isUpper) new IllegalArgumentException("String representing compound term must start with a lowercase letter").failureNel else name.successNel

      val argsVal: ValidationNel[IllegalArgumentException, List[Term]] =

        if(args.isEmpty) new IllegalArgumentException("Argument list in compound term must be not empty").failureNel
        else args.foldRight(List.empty[Term].successNel[IllegalArgumentException])((el: ValidationNel[IllegalArgumentException, Term], acc: ValidationNel[IllegalArgumentException, List[Term]]) => (acc |@| el)((a: List[Term], b: Term) => b :: a))

        (nameVal1 |@| nameVal2 |@| nameVal3 |@| argsVal)((name, _, _, args) => StructImpl(name, args))
    }
  }

  //noinspection FindEmptyCheck
  implicit def fromString(name: String): ValidationNel[IllegalArgumentException, Term] = {
    val nameVal1: ValidationNel[IllegalArgumentException, String] =
      if(name.isEmpty) new IllegalArgumentException("String representing term must be not empty").failureNel else name.successNel
    val nameVal2: ValidationNel[IllegalArgumentException, String] =
      if(name.find(!_.isLetter).isDefined) new IllegalArgumentException("String representing term must contain only letter").failureNel else name.successNel
    (nameVal1 |@| nameVal2)((name, _) => if(name.charAt(0).isLower) AtomImpl(name) else VariableImpl(name))
  }

  implicit def fromInt(value: scala.Int): ValidationNel[IllegalArgumentException, Term] = IntImpl(value).asInstanceOf[Term].successNel

}
