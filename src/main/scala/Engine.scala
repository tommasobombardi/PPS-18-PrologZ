import scalaz._
import Scalaz._
import Clause.{Clause, Fact}

object Engine {

  private var theory: List[ValidationNel[IllegalArgumentException, Clause]] = Nil

  def addTheory(clauses: ValidationNel[IllegalArgumentException, Clause]*): Unit = theory = theory |+| clauses.toList
  def resetTheory(): Unit = theory = Nil

  def solve(theory: List[ValidationNel[IllegalArgumentException, Clause]], goals: List[ValidationNel[IllegalArgumentException, Fact]]): Unit = ??? // TO DO step by step
  def solveAll(theory: List[ValidationNel[IllegalArgumentException, Clause]], goals: List[ValidationNel[IllegalArgumentException, Fact]]): Unit = {
    val theoryVal: List[ValidationNel[String, Clause]] = theory.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[IllegalArgumentException]), index) => nel.map(err => "Error in Clause " + (index + 1) + ": " + err.getMessage).failure
      case (Success(a), _) => a.successNel
    })
    val goalsVal: List[ValidationNel[String, Fact]] = goals.zipWithIndex.map({
      case (Failure(nel: NonEmptyList[IllegalArgumentException]), index) => nel.map(err => "Error in Goal " + (index + 1) + ": " + err.getMessage).failure
      case (Success(a), _) => a.successNel
    })
    val programVal: ValidationNel[String, (List[Clause], List[Fact])] = {
      (theoryVal.foldLeft(List.empty[Clause].successNel[String])((accumulator, element) => (accumulator |@| element)((acc, el) => el :: acc))
      |@| goalsVal.foldLeft(List.empty[Fact].successNel[String])((accumulator, element) => (accumulator |@| element)((acc, el) => el :: acc)))((_, _))
    }
    programVal match {
      case Failure(e: NonEmptyList[String]) => e.foreach(println(_))
      case Success(a) => ((a._1) |+| (a._2)).foreach(c => println(c.toProlog)) // TO DO
    }
  }

}
