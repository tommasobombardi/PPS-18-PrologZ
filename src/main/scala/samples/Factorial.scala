package samples

import prologz.dsl._
import prologz.dsl.ClauseImplicits._
import prologz.dsl.TermImplicits._

object Factorial extends App {

  val s = Struct("s")
  val sum = Predicate("sum")
  val mul = Predicate("mul")
  val factorial = Predicate("factorial")

  Engine.addTheory(
    sum("X",0,"X"),
    sum("X",s("Y"),s("Z")) :- sum("X","Y","Z"),
    mul("X",0,0),
    mul("X",s("Y"),"Z") :- (mul("X","Y","W"), sum("X","W","Z")),
    Predicate("dec")(s("X"),"X"),
    factorial(s("X"),"Y") :- (factorial("X","Z"), mul(s("X"),"Z","Y")),
    factorial(s(0),s(0)))

  Engine.setPrintTree(true)

  Engine.solve(mul(s(0),s(0),"Y"))
  Engine.solve(mul(s(s(s(s(0)))),s(s(s(0))),"Y"))
  Engine.solve(factorial(s(s(s(s(0)))),"Y"))

}
