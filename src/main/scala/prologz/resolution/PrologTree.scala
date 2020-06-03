package prologz.resolution

import scalaz.{Show, TreeLoc}
import scalaz.syntax.monoid._
import scalaz.syntax.tree._
import prologz.dsl.{Clause, Fact}
import prologz.resolution.Implicits.RichFactList
import prologz.resolution.Substitution.{RichSubstitution, Substitution, substitutionMonoid}
import prologz.resolution.Unification.RichClause

/** Helpers for prolog tree construction */
private[prologz] object PrologTree {

  /** Creates a prolog tree
   *
   *  @param theory theory of the prolog program
   *  @param goals goals of the prolog program
   *  @return tree containing only the root node, with theory, goals and an identity substitution
   */
  def initializePrologTree(theory: List[Clause], goals: List[Fact]): TreeLoc[(List[Clause], List[Fact], Substitution)] =
    (theory, goals, Substitution.base(goals.getVariables)).leaf.loc

  /** Navigates the prolog tree until reaching a valid solution or the end of computation
   *
   *  @param theory theory of the prolog program
   *  @param tree initial tree or tree reached with last computation step
   *  @return tree after reaching a valid solution or the end of computation
   */
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
