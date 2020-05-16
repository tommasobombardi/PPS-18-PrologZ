package prologz

import scalaz._
import Scalaz._
import prologz.Clause._
import prologz.Term._

object Engine {

  private var theory: List[ValidationNel[String @@ InputError, Clause]] = Nil

  def addTheory(clauses: ValidationNel[String @@ InputError, Clause]*): Unit = theory = theory |+| clauses.toList
  def resetTheory(): Unit = theory = Nil

  def solve(theory: List[ValidationNel[String @@ InputError, Clause]], goals: List[ValidationNel[String @@ InputError, Fact]]): Unit = validateProgram(theory, goals) match {
    case Failure(err: NonEmptyList[String @@ InputError]) => err.foreach(e => println(Tag.unwrap(e)))
    case Success(p) => (p._1 |+| p._2).foreach(c => println(c.toProlog)) // TO DO step by step
  }
  def solveAll(theory: List[ValidationNel[String @@ InputError, Clause]], goals: List[ValidationNel[String @@ InputError, Fact]]): Unit = validateProgram(theory, goals) match {
    case Failure(err: NonEmptyList[String @@ InputError]) => err.foreach(e => println(Tag.unwrap(e)))
    case Success(p) => (p._1 |+| p._2).foreach(c => println(c.toProlog)) // TO DO
  }

  private def validateProgram(theory: List[ValidationNel[String @@ InputError, Clause]], goals: List[ValidationNel[String @@ InputError, Fact]]): ValidationNel[String @@ InputError, (List[Clause], List[Fact])] = {
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
