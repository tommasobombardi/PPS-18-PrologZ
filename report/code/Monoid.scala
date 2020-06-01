trait Zero[Z] {
  val zero: Z
}
trait Monoid[M] extends Zero[M] with Semigroup[M]

List(1, 2, 3) |+| List(4, 5, 6) // List(1, 2, 3, 4, 5, 6)
"one" mappend "two" // "onetwo"