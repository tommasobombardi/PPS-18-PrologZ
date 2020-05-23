package prologz.resolution

import scalaz._
import Scalaz._
import prologz.dsl.{Clause, Fact}
import prologz.resolution.PrologImplicits._
import prologz.resolution.Substitution._
import prologz.resolution.Unification._

private[prologz] object TreeManager {

  def initializePrologTree(theory: List[Clause], goals: List[Fact]): TreeLoc[(List[Clause], List[Fact], Substitution)] =
    (theory, goals, Substitution.base(goals.getVariables)).leaf.loc

  def searchPrologTree(theory: List[Clause], tree: TreeLoc[(List[Clause], List[Fact], Substitution)]): TreeLoc[(List[Clause], List[Fact], Substitution)] =
    if(tree.isRoot) navigatePrologTree(theory, tree) else navigatePrologTree(theory, tree.parent.get) // backtracking (in case of leaf node with valid solution)

  @scala.annotation.tailrec
  private def navigatePrologTree(theory: List[Clause], tree: TreeLoc[(List[Clause], List[Fact], Substitution)]): TreeLoc[(List[Clause], List[Fact], Substitution)] = tree.getLabel match {
    case (clause :: otherClauses, goal :: otherGoals, subs) =>
      var currentNode: TreeLoc[(List[Clause], List[Fact], Substitution)] = tree.setLabel(otherClauses, goal :: otherGoals, subs)
      currentNode = clause.unify(goal, otherGoals).map(res => currentNode.insertDownLast((theory, res._2, subs |+| res._1).leaf)).getOrElse(currentNode)
      navigatePrologTree(theory, currentNode)
    case (_, _ :: _, _) if tree.parent.isDefined => navigatePrologTree(theory, tree.parent.get) // automatic backtracking (in case of leaf node without valid solution)
    case (_, goals, subs) => tree.setLabel(Nil, goals, subs) // valid solution (in case of leaf node) or  execution completed (in case of root node)
  }

  /** Policy for tree drawing */
  implicit val showPrologTree: Show[(List[Clause], List[Fact], Substitution)] = Show.shows(el => {
    el._2.map(_.toProlog.dropRight(1)).mkString(",") + " || " + el._3.getResult.toProlog
  })

}
