trait Semigroup[S] {
  def append(s1: S, s2: => S): S
}
trait SemigroupOps[A] extends Ops[A] {
  final def |+|(other: => A): A = A.append(self, other)
  final def mappend(other: => A): A = A.append(self, other)
}
