package prologz.core

import scalaz._
import Scalaz._
import scala.language.implicitConversions
import prologz.core.Validation.{InputError, PzValidation}

object TermGenerator {

  object Struct {
    def apply(name: String): PzValidation[String @@ Compound] = {
      val nameVal1: PzValidation[String] =
        if(name.nonEmpty) name.successNel
        else InputError("An empty string is not valid to represent a compound term").failureNel
      val nameVal2: PzValidation[String] =
        if(name.toCharArray.forall(_.isLetter)) name.successNel
        else InputError("String '" + name + "' is not valid to represent a compound term, because it doesn't contain only letters").failureNel
      val nameVal3: PzValidation[String] =
        if(name.nonEmpty && name.charAt(0).isLower) name.successNel
        else InputError("String '" + name + "' is not valid to represent a compound term, because it doesn't start with a lowercase letter").failureNel
      (nameVal1 |@| nameVal2 |@| nameVal3)((name, _, _) => Tag[String, Compound](name))
    }
  }

  implicit class FunctorRich(base: PzValidation[String @@ Compound]) {
    def apply(args: PzValidation[Term]*): PzValidation[Term] = {
      val argsVal: PzValidation[List[Term]] =
        if(args.nonEmpty) args.foldLeft(List.empty[Term].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
        else InputError("Body (namely the list of arguments) of a compound term must be not empty").failureNel
      (base |@| argsVal)((functor, args) => StructImpl(Tag.unwrap(functor), args))
    }
  }

  implicit def fromString(name: String): PzValidation[Term] = {
    val nameVal1: PzValidation[String] =
      if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a term").failureNel
    val nameVal2: PzValidation[String] =
      if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError("String '" + name + "' is not valid to represent a term, because it doesn't contain only letters").failureNel
    (nameVal1 |@| nameVal2)((name, _) => if(name.charAt(0).isLower) AtomImpl(name) else VariableImpl(name))
  }

  implicit def fromInt(value: scala.Int): PzValidation[Term] = AtomImpl(value).asInstanceOf[Term].successNel

  implicit def fromDouble(value: scala.Double): PzValidation[Term] = AtomImpl(value).asInstanceOf[Term].successNel

}
