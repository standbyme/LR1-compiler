package StandbyMe.compiler

import StandbyMe.compiler.universal.Node

class Env(val up: Option[Env], val current: Map[String, Node]) {
  def get(key: String): Option[Node] = {
    current.get(key) match {
      case None => up.flatMap(_.get(key))
      case v => v
    }
  }

  def set(key: String, value: Node): Env = {
    Env(up, current.updated(key, value))
  }
}

object Env {
  def apply(up: Option[Env], current: Map[String, Node]): Env = new Env(up, current)
}