trait Plus[A] {
	def plus(a1: A, a2: A): A
}
def plus[A: Plus](a1: A, a2: A): A =
	implicitly[Plus[A]].plus(a1, a2)

implicit object MyInt extends Plus[Int] {
	override def plus(a1: Int, a2: Int): Int = a1 + a2
}
plus(1, 2)	// 3
