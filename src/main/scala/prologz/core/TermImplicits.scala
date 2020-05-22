package prologz.core

import scalaz._
import Scalaz._
import scala.language.implicitConversions
import prologz.core.Validation.{InputError, PzValidation}

/** Implicit conversions and helpers for [[prologz.core.Term]] instances */
object TermImplicits {

  implicit class FunctorRich(base: PzValidation[String @@ PzFunctor]) {
    /** Creates a compound term, the instance of this class is the functor
     *
     *  @param args terms, which still need to be validated
     *  @return the compound term if there are no errors in args, errors list otherwise
     */
    def apply(args: PzValidation[Term]*): PzValidation[Term] = {
      val argsVal: PzValidation[List[Term]] =
        if(args.nonEmpty) args.foldLeft(List.empty[Term].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
        else InputError("Body (namely the list of arguments) of a compound term must be not empty").failureNel
      (base |@| argsVal)((functor, args) => StructImpl(Tag.unwrap(functor), args))
    }
  }

  /** Coverts a name into a term
   *
   *  @param name term name
   *  @return variable or constant term if there are no errors in name, errors list otherwise
   */
  implicit def fromString(name: String): PzValidation[Term] = {
    val nameVal1: PzValidation[String] =
      if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a term").failureNel
    val nameVal2: PzValidation[String] =
      if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError("String '" + name + "' is not valid to represent a term, because it doesn't contain only letters").failureNel
    (nameVal1 |@| nameVal2)((name, _) => if(name.charAt(0).isLower) AtomImpl(name) else VariableImpl(name))
  }

  /** Converts an integer into a term
   *
   * @param value integer value
   * @return value converted into a term
   */
  implicit def fromInt(value: scala.Int): PzValidation[Term] = AtomImpl(value).asInstanceOf[Term].successNel

  /** Converts a double into a term
   *
   *  @param value double value
   *  @return value converted into a term
   */
  implicit def fromDouble(value: scala.Double): PzValidation[Term] = AtomImpl(value).asInstanceOf[Term].successNel

}
