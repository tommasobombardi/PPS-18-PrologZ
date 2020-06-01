trait Functor[F[_]]  {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
trait FunctorOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Functor[F]
  final def map[B](f: A => B): F[B] = F.map(self)(f)
}

List(1, 2, 3) map {_ + 1} // List(2, 3, 4)
(((_: Int) * 3) map {_ + 100}) (1) // 103
(((x: Int) => x + 1) map {_ * 7}) (3) // 28