sealed trait NonEmptyList[A] {
  val head: A
  val tail: List[A]
  def <::[AA >: A](b: AA): NonEmptyList[AA] = nel(b, head :: tail)
}

1.wrapNel	// NonEmptyList(1)
"error message".wrapNel	// NonEmptyList("error message")
