trait Equal[A] {
	def equal(a1: A, a2: A): Boolean
}

1 === 1	// true
1 == "1"	// false
1 === "1"	// error:type mismatch; found:String("1"); required:Int
1.some =/= 2.some	// true
1.some =/= "2".some	// error:type mismatch; found:Option[String]; required:Option[Int]
1 assert_=== 1	// ok
1 assert_=== 2	// java.lang.RuntimeException: 1 != 2
1 assert_=== "1"	// cannot prove that String <:< Int
