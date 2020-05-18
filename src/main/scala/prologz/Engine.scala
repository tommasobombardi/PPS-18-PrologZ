package prologz

import scalaz._
import Scalaz._
import scala.io.StdIn.readLine
import prologz.Clause.{Clause, Fact}
import prologz.Substitution._
import prologz.Term.Variable
import prologz.Unification._
import prologz.Validation.{InputError,validateProgram}

object Engine {

  private var solved: Int = 0
  private var printTree: Boolean = false
  private var theory: List[ValidationNel[String @@ InputError, Clause]] = Nil

  def addTheory(clauses: ValidationNel[String @@ InputError, Clause]*): Unit = theory = theory |+| clauses.toList
  def resetTheory(): Unit = theory = Nil

  def setPrintTree(status: Boolean): Unit = printTree = status

  def solve(goals: ValidationNel[String @@ InputError, Fact]*): Unit = solveProgram(theory, goals.toList, stepByStep = true)

  def solveAll(goals: ValidationNel[String @@ InputError, Fact]*): Unit = solveProgram(theory, goals.toList, stepByStep = false)

  @scala.annotation.tailrec
  private def constructPrologTree(theory: List[Clause], tree: TreeLoc[(List[Clause], List[Fact], Substitution)]): TreeLoc[(List[Clause], List[Fact], Substitution)] = tree.getLabel match {
    case (clause :: otherClauses, goal :: otherGoals, subs) =>
      var currentNode: TreeLoc[(List[Clause], List[Fact], Substitution)] = tree.setLabel(otherClauses, goal :: otherGoals, subs)
      currentNode = clause.unify(goal, otherGoals).map(res => currentNode.insertDownLast((theory, res._2, subs |+| res._1).leaf)).getOrElse(currentNode)
      constructPrologTree(theory, currentNode)
    case (_, _ :: _, _) if tree.parent.isDefined => constructPrologTree(theory, tree.parent.get) // automatic backtracking (in case of leaf node without valid solution)
    case (_, goals, subs) => tree.setLabel(Nil, goals, subs) // valid solution (in case of leaf node) or  execution completed (in case of root node)
  }

  private implicit def showPrologTree(goalsVariables: Set[Variable]): Show[(List[Clause], List[Fact], Substitution)] = Show.shows(el => {
    el._2.map(_.toProlog.dropRight(1)).mkString(",") + " || " + el._3.getResult(goalsVariables).toProlog
  })

  private def solveProgram(theory: List[ValidationNel[String @@ InputError, Clause]], goals: List[ValidationNel[String @@ InputError, Fact]], stepByStep: Boolean): Unit = validateProgram(theory, goals) match {
    case Failure(err: NonEmptyList[String @@ InputError]) =>
      solved += 1
      println("[PROLOGZ ENGINE] Error report of program " + solved)
      err.foreach(e => println("[PROLOGZ ENGINE] " + Tag.unwrap(e)))
      println()
    case Success(p) =>
      solved += 1
      println("[PROLOGZ ENGINE] Resolution of program " + solved)
      val tree: TreeLoc[(List[Clause], List[Fact], Substitution)] = constructPrologTree(p._1, (p._1, p._2, Substitution()).leaf.loc)
        .whileDo(node => constructPrologTree(p._1, node.parent.get), /* backtracking (in case of leaf node with valid solution) */ node => {
          if (node.getLabel._2.nonEmpty) println("[PROLOGZ ENGINE] Execution completed, all alternatives have been explored")
          else {
            val subs = node.getLabel._3.getResult(p._2.getVariables)
            println("[PROLOGZ ENGINE] Available solution: " + subs.toProlog)
            println("[PROLOGZ ENGINE] Available solution: " + p._2.map(_.substitute(subs)).map(_.toProlog.dropRight(1)).mkString(","))
          }
          !node.isRoot && (!stepByStep || { println("[PROLOGZ ENGINE] Other alternatives can be explored. Next/Accept? (N/A)"); val in = readLine.toLowerCase; in == "n" || in == "next" })
        })
      if(printTree) { println("[PROLOGZ ENGINE] Prolog tree created during resolution"); println(tree.toTree.drawTree(p._2.getVariables)) }
      println()
  }

}
