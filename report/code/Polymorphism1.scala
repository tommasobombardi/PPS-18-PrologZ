def head[A](xs: List[A]): A = xs(0)
head(1 :: 2 :: Nil)	// 1
head("a" :: "b" :: "c" :: Nil)	// "a"
