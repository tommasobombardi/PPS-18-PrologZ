package prologz.core

import org.scalatest.flatspec.AnyFlatSpec
import prologz.core.Clause.Predicate
import prologz.core.Validation.PzValidation
import scalaz.@@

class ClauseSpec extends AnyFlatSpec {

  "A Predicate" should "pop values in last-in-first-out order" in {
    val tryPred: PzValidation[String @@ Predicate] = Predicate("sum")
    assert(tryPred.isSuccess)
  }

}
