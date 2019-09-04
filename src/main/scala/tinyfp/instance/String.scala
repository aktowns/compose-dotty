package tinyfp.instance

import tinyfp.typeclass._

given StringMonoid as Monoid[String]:
  def (x: String) <> (y: String): String = x + y

  def mempty: String = ""