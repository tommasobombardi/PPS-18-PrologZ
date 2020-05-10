import scalaz._
import Scalaz._
import Clause.{Clause, Fact, Predicate}
import Term.Struct

object Engine extends App {

  val sum = Predicate("sum")
  val mul = Predicate("mul")
  val factorial = Predicate("factorial")
  val mathTheory = List[ValidationNel[IllegalArgumentException, Clause]](
    sum("X",0,"X"),
    sum("X",Struct("s")("Y"),Struct("s")("Z")) :- sum("X","Y","Z"),
    mul("X",0,0),
    mul("X",Struct("s")("Y"),"Z") :- (mul("X","Y","W"), sum("X","W","Z")),
    Predicate("dec")(Struct("s")("X"),"X"),
    factorial(Struct("s")("X"),"Y") :- (factorial("X","Z"), mul(Struct("s")("X"),"Z","Y")),
    factorial(Struct("s")(0),Struct("s")(0)))

  def solve(theory: List[ValidationNel[IllegalArgumentException, Clause]], goals: List[ValidationNel[IllegalArgumentException, Fact]]): Unit = {
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

  solve(mathTheory, Nil)

}
