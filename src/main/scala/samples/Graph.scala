package samples

import prologz.dsl._
import prologz.dsl.ClauseImplicits._
import prologz.dsl.TermImplicits._

object Graph extends App {

  val edge = Predicate("edge")
  val reachable = Predicate("reachable")

  Engine.addTheory(
    edge(1, 3),
    edge(1, 5),
    edge(2, 4),
    edge(3, 2),
    edge(3, 5),
    edge(5, 4),
    edge(6, 1),
    edge(6, 5),
    reachable("X", "Y") :- edge("X", "Y"),
    reachable("X", "Y") :- (edge("X", "Z"), reachable("Z", "Y")))

  Engine.solveAll(reachable(1, 4))
  Engine.solveAll(reachable(3, "X"))
  Engine.solve(reachable(6, "X"))

}
