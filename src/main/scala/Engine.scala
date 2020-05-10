import scalaz._
import Scalaz._
import Clause.{Clause, Fact}

object Engine {

  private var theory: List[ValidationNel[IllegalArgumentException, Clause]] = Nil

  def addTheory(clauses: ValidationNel[IllegalArgumentException, Clause]*): Unit = theory = theory |+| clauses.toList
  def resetTheory(): Unit = theory = Nil

  def solve(theory: List[ValidationNel[IllegalArgumentException, Clause]], goals: List[ValidationNel[IllegalArgumentException, Fact]]): Unit = validateProgram(theory, goals) match {
    case Failure(err: NonEmptyList[IllegalArgumentException]) => err.foreach(e => println(e.getMessage))
    case Success(p) => (p._1 |+| p._2).foreach(c => println(c.toProlog)) // TO DO step by step
  }
  def solveAll(theory: List[ValidationNel[IllegalArgumentException, Clause]], goals: List[ValidationNel[IllegalArgumentException, Fact]]): Unit = validateProgram(theory, goals) match {
    case Failure(err: NonEmptyList[IllegalArgumentException]) => err.foreach(e => println(e.getMessage))
    case Success(p) => (p._1 |+| p._2).foreach(c => println(c.toProlog)) // TO DO
  }

  private def validateProgram(theory: List[ValidationNel[IllegalArgumentException, Clause]], goals: List[ValidationNel[IllegalArgumentException, Fact]]): ValidationNel[IllegalArgumentException, (List[Clause], List[Fact])] = {
    val theoryReadableVal: List[ValidationNel[IllegalArgumentException, Clause]] = theory.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[IllegalArgumentException]), index) => nel.map(err => new IllegalArgumentException("Error in Clause " + (index + 1) + ": " + err.getMessage)).failure
      case (Success(a), _) => a.successNel
    })
    val goalsReadableVal: List[ValidationNel[IllegalArgumentException, Fact]] = goals.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[IllegalArgumentException]), index) => nel.map(err => new IllegalArgumentException("Error in Goal " + (index + 1) + ": " + err.getMessage)).failure
      case (Success(a), _) => a.successNel
    })
    (theoryReadableVal.foldLeft(List.empty[Clause].successNel[IllegalArgumentException])((accumulator, element) => (accumulator |@| element)((acc, el) => el :: acc))
    |@| goalsReadableVal.foldLeft(List.empty[Fact].successNel[IllegalArgumentException])((accumulator, element) => (accumulator |@| element)((acc, el) => el :: acc)))((_, _))
  }

}
