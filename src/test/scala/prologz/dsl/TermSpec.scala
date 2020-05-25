package prologz.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TermSpec extends AnyFlatSpec with Matchers {

  "The module for implicit term creation" should "covert a double into a valid constant term" in {

  }

  it should "convert an integer into a valid constant term" in {

  }

  it should "convert a string without errors and starting with a lowercase letter into a valid constant term" in {

  }

  it should "convert a string without errors and starting with an uppercase letter into a valid variable term" in {

  }

  it should "convert a string containing errors into an error list" in {

  }

  "The functor factory" should "create a valid functor in case of valid functor name" in {

  }

  it should "retrieve an error list in case of errors in functor name" in {

  }

  "The use of a functor" should "create a valid compound term in case of valid functor and valid argument list" in {

  }

  it should "retrieve an error list in case of errors in functor or in argument list" in {

  }

}
