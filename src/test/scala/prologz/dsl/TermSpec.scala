package prologz.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.TermImplicits.{FunctorRich, fromDouble, fromInt, fromString}

class TermSpec extends AnyFlatSpec with Matchers {

  "The module for implicit term creation" should "covert a double into a valid constant term" in {
    fromDouble(2.5) shouldBe Symbol("isSuccess")
    fromDouble(3.14) shouldBe Symbol("isSuccess")
  }

  it should "convert an integer into a valid constant term" in {
    fromInt(1) shouldBe Symbol("isSuccess")
    fromInt(20) shouldBe Symbol("isSuccess")
  }

  it should "convert a string without errors and starting with a lowercase letter into a valid constant term" in {
    fromString("a") shouldBe Symbol("isSuccess")
    fromString("zero") shouldBe Symbol("isSuccess")
  }

  it should "convert a string without errors and starting with an uppercase letter into a valid variable term" in {
    fromString("X") shouldBe Symbol("isSuccess")
    fromString("VarY") shouldBe Symbol("isSuccess")
  }

  it should "convert a string containing errors into an error list" in {
    fromString("") shouldBe Symbol("isFailure")
    fromString("12b") shouldBe Symbol("isFailure")
    fromString("Variable:_") shouldBe Symbol("isFailure")
  }

  "The functor factory" should "create a valid functor in case of valid functor name" in {
    Struct("p") shouldBe Symbol("isSuccess")
    Struct("seq") shouldBe Symbol("isSuccess")
  }

  it should "retrieve an error list in case of errors in functor name" in {
    Struct("Seq") shouldBe Symbol("isFailure")
    Struct("p:1") shouldBe Symbol("isFailure")
  }

  "The use of a functor" should "create a valid compound term in case of valid functor and valid argument list" in {
    Struct("p")(1, "uno") shouldBe Symbol("isSuccess")
    Struct("seq")("X", "Y") shouldBe Symbol("isSuccess")
  }

  it should "retrieve an error list in case of errors in functor or in argument list" in {
    Struct("p:1")("X", "uno") shouldBe Symbol("isFailure")
    Struct("p")("X2", "uno1:") shouldBe Symbol("isFailure")
    Struct("p:1")("X2", "uno1:") shouldBe Symbol("isFailure")
  }

}
