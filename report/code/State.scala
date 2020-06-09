type State[S,A] = StateT[Id,S,A]
object State extends StateFunctions {
	def apply[S,A](f: S => (S,A)): State[S,A] = new StateT[Id,S,A] {
		def apply(s: S) = f(s)
	}
}
for {
  a <- State[Int,String] { s => (s+1, s"1st result is ${s+1}") }
  b <- State[Int,String] { s => (s*2, s"2nd result is ${s*2}") }
  c <- State[Int,String] { s => (s%10, s"3rd result is ${s%10}") }
} yield a + b + c
