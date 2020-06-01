trait Apply[F[_]] extends Functor[F] {
  def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]
}
trait Applicative[F[_]] extends Apply[F] {
  def point[A](a: => A): F[A]
  def pure[A](a: => A): F[A] = point(a)
}

9.some <*> {(_: Int) + 3}.some // Some(12)
3.some <*> { 9.some <*> {(_: Int) + (_: Int)}.curried.some }
	// Some(12)
List(1, 2, 3) <*> List((_: Int) * 0, (_: Int) + 100, x => x * x)
	// List(0, 0, 0, 101, 102, 103, 1, 4, 9)
List(3, 4) <*> { List(1, 2) <*> List({(_: Int) + (_: Int)}.curried, {(_: Int) * (_: Int)}.curried) }
	// List(4, 5, 5, 6, 3, 4, 6, 8)