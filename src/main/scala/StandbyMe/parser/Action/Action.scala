package StandbyMe.parser.Action

import StandbyMe.parser.Production

sealed trait Action

case class S(value: Int) extends Action

case class r(value: Production) extends Action

case class acc() extends Action