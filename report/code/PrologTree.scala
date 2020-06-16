def searchPrologTree(theory: List[Clause],
	tree: TreeLoc[(List[Clause],List[Fact],Substitution)])
	: TreeLoc[(List[Clause],List[Fact],Substitution)] =
 if(tree.isRoot) navigatePrologTree(theory, tree)
 else navigatePrologTree(theory, tree.parent.get)
 // backtracking (leaf node with valid solution)

@scala.annotation.tailrec
private def navigatePrologTree(theory: List[Clause],
	tree: TreeLoc[(List[Clause],List[Fact],Substitution)])
	: TreeLoc[(List[Clause],List[Fact],Substitution)] =
 tree.getLabel match {
  case (clause :: otherClauses, goal :: otherGoals, subs) =>
   var currentNode: TreeLoc[(List[Clause],List[Fact],Substitution)]
    = tree.setLabel(otherClauses, goal :: otherGoals, subs)
   currentNode = clause.unify(goal, otherGoals).map(res =>
    currentNode.insertDownLast((theory, res._2, subs |+| res._1)
    .leaf)).getOrElse(currentNode)
   navigatePrologTree(theory, currentNode)
  case (_, _ :: _, _) if tree.parent.isDefined =>
   navigatePrologTree(theory, tree.parent.get)
   // automatic backtracking (leaf node without valid solution)
  case (_, goals, subs) => tree.setLabel(Nil, goals, subs)
   // valid solution (leaf node) or execution completed (root node)
 }
