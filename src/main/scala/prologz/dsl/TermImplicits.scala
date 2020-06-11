package prologz.dsl

import scalaz.{@@, Equal, Tag}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scala.language.implicitConversions
import prologz.resolution.InputError
import prologz.resolution.Validation.{InputError, PzValidation}

/** Implicit conversions and helpers for [[prologz.dsl.Term]] instances */
object TermImplicits {

  implicit class FunctorRich(base: PzValidation[String @@ Functor]) {
    /** Creates a compound term, the instance of this class is the functor
     *
     *  @param args terms, which still need to be validated
     *  @return the compound term if there is no error in functor or args, errors list otherwise
     */
    def apply(args: PzValidation[Term]*): PzValidation[Struct] = {
      val argsVal: PzValidation[List[Term]] =
        if(args.nonEmpty) args.foldLeft(List.empty[Term].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
        else InputError("Body (namely the list of arguments) of a compound term must be not empty").failureNel
      (base |@| argsVal)((functor, args) => Struct(Tag.unwrap(functor), args))
    }
  }

  /** Coverts a name into a term
   *
   *  @param name term name
   *  @return variable or constant term if there is no error in name, errors list otherwise
   */
  implicit def fromString(name: String): PzValidation[Term] = {
    val nameVal1: PzValidation[String] = if(name.nonEmpty) name.successNel
      else InputError("An empty string is not valid to represent a term").failureNel
    val nameVal2: PzValidation[String] = if(name.toCharArray.forall(_.isLetter)) name.successNel
      else InputError(s"String '$name' is not valid to represent a term, because it doesn't contain only letters").failureNel
    (nameVal1 |@| nameVal2)((name, _) => if(name.charAt(0).isLower) Atom(name) else Variable(name))
  }

  /** Converts an integer into a term
   *
   * @param value integer value
   * @return value converted into a term
   */
  implicit def fromInt(value: Int): PzValidation[Atom[Int]] = Atom(value).successNel

  /** Converts a double into a term
   *
   *  @param value double value
   *  @return value converted into a term
   */
  implicit def fromDouble(value: Double): PzValidation[Atom[Double]] = Atom(value).successNel

  private[prologz] implicit object TermEqual extends Equal[Term] {
    override def equal(a1: Term, a2: Term): Boolean = a1 == a2
  }

}
