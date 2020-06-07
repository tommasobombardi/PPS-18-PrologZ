trait Plus[A] {
	def plus(a2: A): A
}
def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)

case class MyInt(value: Int) extends Plus[MyInt] {
	override def plus(a2: MyInt): MyInt = MyInt(value+a2.value)
}
plus(MyInt(1), MyInt(2))  // MyInt(3)
