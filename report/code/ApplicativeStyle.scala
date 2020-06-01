^(3.some, 5.some) {_ + _} // Option[Int] = Some(8)
^(3.some, none[Int]) {_ + _} // None
(3.some |@| 5.some) {_ + _} // Option[Int] = Some(8)
(List("ha", "heh", "hmm") |@| List("?", "!", ".")) {_ + _}
	// List(ha?, ha!, ha., heh?, heh!, heh., hmm?, hmm!, hmm.)