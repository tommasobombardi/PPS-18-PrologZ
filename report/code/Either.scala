for {
	e1 <- "event 1 ok".right
	e2 <- "event 2 failed!".left[String]
	e3 <- "event 3 failed!".left[String]
} yield (e1 |+| e2 |+| e3)	// -\/("event 2 failed!")