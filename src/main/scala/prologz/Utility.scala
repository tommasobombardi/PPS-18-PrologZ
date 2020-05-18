package prologz

import scalaz._
import Scalaz._
import prologz.Clause.{Clause, Fact}

private[prologz] object Utility {

  sealed trait InputError
  def InputError(message: String): String @@ InputError = Tag[String, InputError](message)

  def validateProgram(theory: List[ValidationNel[String @@ InputError, Clause]], goals: List[ValidationNel[String @@ InputError, Fact]]): ValidationNel[String @@ InputError, (List[Clause], List[Fact])] = {
    val theoryReadableVal: List[ValidationNel[String @@ InputError, Clause]] = theory.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[String @@ InputError]), index) => nel.map(err => InputError("Error in Clause " + (index + 1) + ": " + Tag.unwrap(err))).failure
      case (Success(a), _) => a.successNel
    })
    val goalsReadableVal: List[ValidationNel[String @@ InputError, Fact]] = goals.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[String @@ InputError]), index) => nel.map(err => InputError("Error in Goal " + (index + 1) + ": " + Tag.unwrap(err))).failure
      case (Success(a), _) => a.successNel
    })
    (theoryReadableVal.foldLeft(List.empty[Clause].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el))
    |@| goalsReadableVal.foldLeft(List.empty[Fact].successNel[String @@ InputError])((accumulator, element) => (accumulator |@| element)((acc, el) => acc :+ el)))((_, _))
  }

}
