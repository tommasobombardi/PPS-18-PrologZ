package samples

import prologz.dsl._
import prologz.dsl.ClauseImplicits._
import prologz.dsl.TermImplicits._

object Relationship extends App {

  val son = Predicate("son")
  val father = Predicate("father")
  val grandfather = Predicate("grandfather")
  val male = Predicate("male")

  Engine.addTheory(
    son("X","Y") :- (father("Y","X"), male("X")),
    grandfather("X","Z") :- (father("X","Y"), father("Y","Z")),
    father("abraham","isaac"),
    father("terach","abraham"),
    male("isaac"),
    male("abraham"),
    male("terach"))

  Engine.setPrintTree(true)

  Engine.solveAll(grandfather("terach","isaac"))
  Engine.solveAll(grandfather("terach","X"))
  Engine.solveAll(son("X","Y"))

}
