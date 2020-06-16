trait OptionFunctions {
  final def some[A](a: A): Option[A] = Some(a)
  final def none[A]: Option[A] = None
}

1.some // Some(1)
val os = List(Some(42), None, Some(20))
os.foldLeft(None) { (acc, o) => acc orElse o }
	// error:type mismatch; found:Option[Int]; required:None.type
os.foldLeft(None:Option[Int]){(acc, o) => acc orElse o} // Some(42)
os.foldLeft(none[Int]) { (acc, o) => acc orElse o } // Some(42)
