3.some >>= { x => "!".some >>= { y => (none: Option[String]) } }

for {
	x <- 3.some
	y <- "!".some
	z <- none
} yield z