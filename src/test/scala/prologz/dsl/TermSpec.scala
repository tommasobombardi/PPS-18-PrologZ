package prologz.dsl

import scalaz.{@@, ValidationNel}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.TermImplicits.{FunctorRich, fromDouble, fromInt, fromString}
import prologz.resolution.InputError
import prologz.resolution.Validation.PzValidation

class TermSpec extends AnyFlatSpec with Matchers {

  "The module for implicit term creation" should "covert a double into a valid constant term" in {
    fromDouble(2.5) should (be a Symbol("isSuccess") and be (a [PzValidation[Atom[Double]]]))
    fromDouble(3.14) should (be a Symbol("isSuccess") and be (a [PzValidation[Atom[Double]]]))
  }

  it should "convert an integer into a valid constant term" in {
    fromInt(1) should (be a Symbol("isSuccess") and be (a [PzValidation[Atom[Int]]]))
    fromInt(20) should (be a Symbol("isSuccess") and be (a [PzValidation[Atom[Int]]]))
  }

  it should "convert a string without errors and starting with a lowercase letter into a valid constant term" in {
    fromString("a") should (be a Symbol("isSuccess") and be (a [PzValidation[Atom[String]]]))
    fromString("zero") should (be a Symbol("isSuccess") and be (a [PzValidation[Atom[String]]]))
  }

  it should "convert a string without errors and starting with an uppercase letter into a valid variable term" in {
    fromString("X") should (be a Symbol("isSuccess") and be (a [PzValidation[Variable]]))
    fromString("VarY") should (be a Symbol("isSuccess") and be (a [PzValidation[Variable]]))
  }

  it should "convert a string containing errors into an error list" in {
    fromString("") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Term]]))
    fromString("12b") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Term]]))
    fromString("Variable:_") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Term]]))
  }

  "The functor factory" should "create a valid functor in case of valid functor name" in {
    Struct("p") should (be a Symbol("isSuccess") and be (a [PzValidation[String @@ PzFunctor]]))
    Struct("seq") should (be a Symbol("isSuccess") and be (a [PzValidation[String @@ PzFunctor]]))
  }

  it should "retrieve an error list in case of errors in functor name" in {
    Struct("Seq") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, String @@ PzFunctor]]))
    Struct("p:1") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, String @@ PzFunctor]]))
  }

  "The use of a functor" should "create a valid compound term in case of valid functor and valid argument list" in {
    Struct("p")(1, "uno") should (be a Symbol("isSuccess") and be (a [PzValidation[Struct]]))
    Struct("seq")("X", "Y") should (be a Symbol("isSuccess") and be (a [PzValidation[Struct]]))
  }

  it should "retrieve an error list in case of errors in functor or in argument list" in {
    Struct("p:1")("X", "uno") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Struct]]))
    Struct("p")("X2", "uno1:") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Struct]]))
    Struct("p:1")("X2", "uno1:") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Struct]]))
  }

}
