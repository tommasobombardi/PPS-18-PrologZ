package prologz

import scalaz.{@@, Tag}

private[prologz] object Utility {

  sealed trait InputError
  def InputError(message: String): String @@ InputError = Tag[String, InputError](message)

}
