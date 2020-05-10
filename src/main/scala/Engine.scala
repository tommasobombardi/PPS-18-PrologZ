import scalaz._
import Scalaz._
import Clause.{Clause, Predicate}

object Engine extends App {

  val tryVal: List[ValidationNel[IllegalArgumentException, Clause]] = List(Predicate("sum")(1,2))

  val c: List[ValidationNel[IllegalArgumentException, Clause]] = tryVal.zipWithIndex.map( {
    case (Failure(a: NonEmptyList[IllegalArgumentException]), index) => a.map(el => new IllegalArgumentException(index + el.getMessage)).failure
    case (Success(a: Clause), _) => a.successNel
  })

  val program: ValidationNel[IllegalArgumentException, List[Clause]] = c.foldLeft(List.empty[Clause].successNel[IllegalArgumentException])((accumulator, element) => (accumulator |@| element)((acc, el) => el :: acc))

  program match {
    case Failure(a: NonEmptyList[IllegalArgumentException]) => a.foreach(ex => println(ex.getMessage))
    case Success(a) => a.foreach(c => println(c.toProlog))
  }

}