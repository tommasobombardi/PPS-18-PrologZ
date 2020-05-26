package prologz.dsl

import scalaz.{@@, ValidationNel}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.ClauseImplicits.{FactRich, PredicateRich}
import prologz.dsl.TermImplicits.{FunctorRich, fromInt, fromString}
import prologz.resolution.InputError
import prologz.resolution.Validation.PzValidation

class ClauseSpec extends AnyFlatSpec with Matchers {

  "The predicate factory" should "create a valid predicate in case of valid predicate name" in {
    Predicate("sum") should (be a Symbol("isSuccess") and be (a [PzValidation[String @@ Predicate]]))
    Predicate("father") should (be a Symbol("isSuccess") and be (a [PzValidation[String @@ Predicate]]))
  }

  it should "retrieve an error list in case of errors in predicate name" in {
    Predicate("sum++") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, String @@ Predicate]]))
    Predicate("Father") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, String @@ Predicate]]))
  }

  "The use of a predicate" should "create a valid fact in case of valid predicate and valid argument list" in {
    Predicate("sum")("X", 0, "X") should (be a Symbol("isSuccess") and be (a [PzValidation[Fact]]))
    Predicate("father")("isaac", "Y") should (be a Symbol("isSuccess") and be (a [PzValidation[Fact]]))
  }

  it should "retrieve an error list in case of errors in predicate or in argument list" in {
    Predicate("sum++")("X", 0, "X") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Fact]]))
    Predicate("sum")("X1", 0, "_/X") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Fact]]))
    Predicate("sum++")("X1", 0, "_/X") should (be a Symbol("isFailure") and be (a [ValidationNel[String @@ InputError, Fact]]))
  }

  "Applying ':-' or 'setBody' on a fact" should "create a valid rule in case of valid fact (head) and valid argument list (body)" in {
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")) :- Predicate("sum")("X", "Y", "Z") should (be a Symbol("isSuccess") and be (a [PzValidation[Rule]]))
    Predicate("son")("X", "Y") :- (Predicate("son")("Y", "X"), Predicate("male")("X")) should (be a Symbol("isSuccess") and be (a [PzValidation[Rule]]))
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")).setBody(Predicate("sum")("X", "Y", "Z")) should (be a Symbol("isSuccess") and be (a [PzValidation[Rule]]))
    Predicate("son")("X", "Y").setBody(Predicate("son")("Y", "X"), Predicate("male")("X")) should (be a Symbol("isSuccess") and be (a [PzValidation[Rule]]))
  }

  it should "retrieve an error list in case of errors in fact (head) or in argument list (body)" in {

  }

}
