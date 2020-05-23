package prologz.core

import scalaz._
import Scalaz._
import scala.io.StdIn.readLine
import prologz.core.PrologImplicits._
import prologz.core.Substitution._
import prologz.core.Unification._
import prologz.core.Validation.{PzValidation, validateProgram}

/** Prolog engine used to solve programs */
object Engine {

  private var solved: Int = 0
  private var printTree: Boolean = false
  private var theory: List[PzValidation[Clause]] = Nil

  /** Adds clauses to engine theory
   *
   *  @param clauses clauses which must be added to theory
   */
  def addTheory(clauses: PzValidation[Clause]*): Unit = theory = theory |+| clauses.toList

  /** Resets engine theory */
  def resetTheory(): Unit = theory = Nil

  /** Enables or disables the print tree option
   *
   *  @param status true to show prolog tree after program resolution, false otherwise
   */
  def setPrintTree(status: Boolean): Unit = printTree = status

  /** Solves the program step by step
   *
   *  @param goals goals that must be solved
   */
  def solve(goals: PzValidation[Fact]*): Unit = solveProgram(theory, goals.toList, stepByStep = true)

  /** Solves the program in a single step
   *
   *  @param goals goals that must be solved
   */
  def solveAll(goals: PzValidation[Fact]*): Unit = solveProgram(theory, goals.toList, stepByStep = false)

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
  private implicit val showPrologTree: Show[(List[Clause], List[Fact], Substitution)] = Show.shows(el => {
    el._2.map(_.toProlog.dropRight(1)).mkString(",") + " || " + el._3.getResult.toProlog
  })

  private def solveProgram(theory: List[PzValidation[Clause]], goals: List[PzValidation[Fact]], stepByStep: Boolean): Unit = validateProgram(theory, goals) match {
    case Failure(err: NonEmptyList[String @@ InputError]) =>
      solved += 1
      println("[PROLOG ENGINE] Error report of program " + solved)
      err.foreach(e => println("[PROLOG ENGINE] " + Tag.unwrap(e)))
      println()
    case Success(p) =>
      solved += 1
      println("[PROLOG ENGINE] Resolution of program " + solved)
      val tree: TreeLoc[(List[Clause], List[Fact], Substitution)] = navigatePrologTree(p._1, (p._1, p._2, Substitution.base(p._2.getVariables)).leaf.loc)
        .whileDo(node => navigatePrologTree(p._1, node.parent.get), /* backtracking (in case of leaf node with valid solution) */ node => {
          if (node.getLabel._2.nonEmpty) println("[PROLOG ENGINE] Execution completed, all alternatives have been explored")
          else {
            println("[PROLOG ENGINE] Available solution: " + node.getLabel._3.getResult.toProlog)
            println("[PROLOG ENGINE] Available solution: " + p._2.map(_.substitute(node.getLabel._3.getResult)).map(_.toProlog.dropRight(1)).mkString(","))
          }
          !node.isRoot && (!stepByStep || { println("[PROLOG ENGINE] Other alternatives can be explored. Next/Accept? (N/A)")
            val in = readLine.toLowerCase; in == "n" || in == "next" })
        })
      if(printTree) { println("[PROLOG ENGINE] Prolog tree created during resolution"); println(tree.toTree.drawTree) }
      println()
  }

}
