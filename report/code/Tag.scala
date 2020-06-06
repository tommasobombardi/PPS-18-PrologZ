type Tagged[U] = { type Tag = U }
type @@[T, U] = T with Tagged[U]

sealed trait KiloGram
def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
val mass = KiloGram(20.0)
2 * Tag.unwrap(mass) // 40.0
