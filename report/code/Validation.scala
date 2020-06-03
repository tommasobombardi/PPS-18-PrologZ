"event 1 ok".success[String]	// Success("event 1 ok")
"event 1 failed!".failure[String]	// Failure("event 1 failed!")
"event 1 ok".success[String] |@| "event 2 failed!".failure[String] |@| "event 3 failed!".failure[String]) {_ + _ + _}
	// Failure("event 2 failed!event 3 failed!")
