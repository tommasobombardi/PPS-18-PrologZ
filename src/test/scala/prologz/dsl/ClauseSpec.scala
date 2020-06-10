package prologz.dsl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prologz.dsl.ClauseImplicits.{FactRich, PredicateRich}
import prologz.dsl.TermImplicits.{FunctorRich, fromInt, fromString}

class ClauseSpec extends AnyFlatSpec with Matchers {

  "The predicate factory" should "create a valid predicate in case of valid predicate name" in {
    Predicate("sum") shouldBe Symbol("isSuccess")
    Predicate("father") shouldBe Symbol("isSuccess")
  }

  it should "retrieve an error list in case of errors in predicate name" in {
    Predicate("sum++") shouldBe Symbol("isFailure")
    Predicate("Father") shouldBe Symbol("isFailure")
  }

  "The use of a predicate" should "create a valid fact in case of valid predicate and valid argument list" in {
    Predicate("sum")("X", 0, "X") shouldBe Symbol("isSuccess")
    Predicate("father")("isaac", "Y") shouldBe Symbol("isSuccess")
  }

  it should "retrieve an error list in case of errors in predicate or in argument list" in {
    Predicate("sum++")("X", 0, "X") shouldBe Symbol("isFailure")
    Predicate("sum")("X1", 0, "_/X") shouldBe Symbol("isFailure")
    Predicate("sum++")("X1", 0, "_/X") shouldBe Symbol("isFailure")
  }

  "Applying ':-' or 'setBody' on a fact" should "create a valid rule in case of valid fact (head) and valid argument list (body)" in {
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")) :- Predicate("sum")("X", "Y", "Z") shouldBe Symbol("isSuccess")
    Predicate("son")("X", "Y") :- (Predicate("son")("Y", "X"), Predicate("male")("X")) shouldBe Symbol("isSuccess")
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")).setBody(Predicate("sum")("X", "Y", "Z")) shouldBe Symbol("isSuccess")
    Predicate("son")("X", "Y").setBody(Predicate("son")("Y", "X"), Predicate("male")("X")) shouldBe Symbol("isSuccess")
  }

  it should "retrieve an error list in case of errors in fact (head) or in argument list (body)" in {
    Predicate("sum++")("X", Struct("S")("Y"), Struct("s")("Z2")) :- Predicate("sum")("X", "Y", "Z") shouldBe Symbol("isFailure")
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")) :- Predicate("Sum")("X**", "Y", "1Z") shouldBe Symbol("isFailure")
    Predicate("sum++")("X", Struct("S")("Y"), Struct("s")("Z2"))  :- Predicate("Sum")("X**", "Y", "1Z") shouldBe Symbol("isFailure")
    Predicate("sum++")("X", Struct("S")("Y"), Struct("s")("Z2")).setBody(Predicate("sum")("X", "Y", "Z")) shouldBe Symbol("isFailure")
    Predicate("sum")("X", Struct("s")("Y"), Struct("s")("Z")).setBody(Predicate("Sum")("X**", "Y", "1Z")) shouldBe Symbol("isFailure")
    Predicate("sum++")("X", Struct("S")("Y"), Struct("s")("Z2")).setBody(Predicate("Sum")("X**", "Y", "1Z")) shouldBe Symbol("isFailure")
  }

}
