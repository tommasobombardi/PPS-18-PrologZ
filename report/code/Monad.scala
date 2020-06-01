trait Bind[F[_]] extends Apply[F] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}
trait BindOps[F[_],A] extends Ops[F[A]] {
  def flatMap[B](f: A => F[B]) = F.bind(self)(f)
  def >>=[B](f: A => F[B]) = F.bind(self)(f)
}
trait Monad[F[_]] extends Applicative[F] with Bind[F]

Monad[Option].point("WHAT") // Some(WHAT)
3.some flatMap { x => (x + 1).some } // Some(4)
(none: Option[Int]) flatMap { x => (x + 1).some } // None
9.some flatMap { x => Monad[Option].point(x * 10) } // Some(90)
3.some >>= { x => "!".some >>= { y => (none: Option[String]) } }
	// None