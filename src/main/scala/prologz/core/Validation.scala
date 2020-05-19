package prologz.core

import scalaz._
import Scalaz._
import prologz.core.Clause.{Clause, Fact}

private[core] object Validation {

  sealed trait InputError
  def InputError(message: String): String @@ InputError = Tag[String, InputError](message)

  type PzValidation[A] = ValidationNel[String @@ InputError, A]

  def validateProgram(theory: List[PzValidation[Clause]], goals: List[PzValidation[Fact]]): PzValidation[(List[Clause], List[Fact])] = {
    val theoryReadableVal: List[PzValidation[Clause]] = theory.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[String @@ InputError]), index) => nel.map(err => InputError("Error in Clause " + (index + 1) + ": " + Tag.unwrap(err))).failure
      case (Success(a), _) => a.successNel
    })
    val goalsReadableVal: List[PzValidation[Fact]] = goals.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[String @@ InputError]), index) => nel.map(err => InputError("Error in Goal " + (index + 1) + ": " + Tag.unwrap(err))).failure
      case (Success(a), _) => a.successNel
    })
    (theoryReadableVal.foldLeft(List.empty[Clause].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
    |@| goalsReadableVal.foldLeft(List.empty[Fact].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el)))((_, _))
  }

}
