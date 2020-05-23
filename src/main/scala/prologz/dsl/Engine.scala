package prologz.dsl

import scalaz._
import Scalaz._
import scala.io.StdIn.readLine
import prologz.resolution.InputError
import prologz.resolution.Implicits._
import prologz.resolution.Substitution._
import prologz.resolution.PrologTree._
import prologz.resolution.Validation.{PzValidation, validateProgram}

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

  private def solveProgram(theory: List[PzValidation[Clause]], goals: List[PzValidation[Fact]], stepByStep: Boolean): Unit = validateProgram(theory, goals) match {
    case Failure(err: NonEmptyList[String @@ InputError]) =>
      solved += 1
      println("[PROLOG ENGINE] Error report of program " + solved)
      err.foreach(e => println("[PROLOG ENGINE] " + Tag.unwrap(e)))
      println()
    case Success(p) =>
      solved += 1
      println("[PROLOG ENGINE] Resolution of program " + solved)
      val tree: TreeLoc[(List[Clause], List[Fact], Substitution)] = initializePrologTree(p._1, p._2)
        .doWhile(node => searchPrologTree(p._1, node), node => {
          if(node.getLabel._2.isEmpty) {
            println("[PROLOG ENGINE] Available solution: " + node.getLabel._3.getResult.toProlog)
            println("[PROLOG ENGINE] Available solution: " + p._2.map(_.substitute(node.getLabel._3.getResult)).map(_.toProlog.dropRight(1)).mkString(","))
          }
          if(node.isRoot) println("[PROLOG ENGINE] Execution completed, all alternatives have been explored")
          !node.isRoot && (!stepByStep || { println("[PROLOG ENGINE] Other alternatives can be explored. Next/Accept? (N/A)")
            val in = readLine.toLowerCase; in == "n" || in == "next" })
        })
      if(printTree) { println("[PROLOG ENGINE] Prolog tree created during resolution"); println(tree.toTree.drawTree) }
      println()
  }

}
