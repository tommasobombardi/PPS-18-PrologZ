sealed trait Tree[A] {
	def rootLabel: A
	def subForest: Stream[Tree[A]]
}
trait TreeFunctions {
	def leaf[A](root: => A): Tree[A] = node(root, Stream.empty)
	def node[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = new Tree[A] {
    		lazy val rootLabel = root
    		lazy val subForest = forest
    		override def toString = "<tree>"
  	}
}
object Tree extends TreeFunctions with TreeInstances {
  	def apply[A](root: => A): Tree[A] = leaf(root)
}
trait TreeV[A] extends Ops[A] {
	def leaf: Tree[A] = Tree.leaf(self)
	def node(subForest: Tree[A]*): Tree[A] = Tree.node(self, subForest.toStream)
}

'P'.node('O'.node('L'.node('N'.leaf,'T'.leaf),'Y'.node('S'.leaf)),
	 'L'.node('W'.node('C'.leaf,'R'.leaf),'A'.node('A'.leaf)))
