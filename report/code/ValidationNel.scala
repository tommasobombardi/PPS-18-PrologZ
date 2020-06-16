"event 1 ok".successNel[String]
	// Success("event 1 ok")
"event 1 failed!".failureNel[String]
	// Failure(NonEmptyList("event 1 failed!"))
("event 1 ok".successNel[String] |@| "event 2 failed!".failureNel[String] |@| "event 3 failed!".failureNel[String]) {_ + _ + _}
	// Failure(NonEmptyList("event 2 failed!", "event 3 failed!"))
