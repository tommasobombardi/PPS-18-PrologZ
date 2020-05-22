package prologz.examples

import prologz.core.ClauseImplicits._
import prologz.core.Engine.{addTheory, setPrintTree, solve}
import prologz.core.Struct
import prologz.core.TermImplicits._

object Factorial extends App {

  val s = Struct("s")
  val sum = Predicate("sum")
  val mul = Predicate("mul")
  val factorial = Predicate("factorial")

  addTheory(
    sum("X",0,"X"),
    sum("X",s("Y"),s("Z")) :- sum("X","Y","Z"),
    mul("X",0,0),
    mul("X",s("Y"),"Z") :- (mul("X","Y","W"), sum("X","W","Z")),
    Predicate("dec")(s("X"),"X"),
    factorial(s("X"),"Y") :- (factorial("X","Z"), mul(s("X"),"Z","Y")),
    factorial(s(0),s(0)))

  setPrintTree(true)

  solve(mul(s(0),s(0),"Y"))
  solve(mul(s(s(s(s((0))))),s(s(s(0))),"Y"))
  solve(factorial(s(s(s((s(0))))),"Y"))

}
