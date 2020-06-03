package prologz.resolution

import scalaz.{@@, Failure, NonEmptyList, Success, Tag, ValidationNel}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import prologz.dsl.{Clause, Fact}

/** Prolog input error */
sealed trait InputError

/** Helpers for prolog program validation */
private[prologz] object Validation {

  type PzValidation[A] = ValidationNel[String @@ InputError, A]

  /** Creates a prolog input error
   *
   *  @param message error description
   *  @return error tagged with prolog input error type
   */
  def InputError(message: String): String @@ InputError = Tag[String, InputError](message)

  /** Validates a prolog program
   *
   *  @param theory program theory clauses, which still need to be validated
   *  @param goals program goals, which still need to be validated
   *  @return a tuple containing theory and goals if there is no error in the program, errors list otherwise
   */
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
