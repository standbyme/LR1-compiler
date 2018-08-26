package StandbyMe.compiler

import StandbyMe.compiler.universal.{BasicNode, Node, SyntacticSymbol, Token}
import StandbyMe.parser.{Production, Table, compute_Table, LR}
import StandbyMe.lexer.Lexer

object Run extends App {
  val code: String = "if a==1 b=12 else b=16"
  val result = Lexer(code).toVector
  result.foreach(println)
  val init_buffer: Vector[Token] = result ++ Vector((SyntacticSymbol.$, null))
  val production__vector = Vector[Production](
    SyntacticSymbol.STARTER -> Vector(SyntacticSymbol.EXPRESSION),
    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.INT),
    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.PLUS, SyntacticSymbol.EXPRESSION),
    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.ID, SyntacticSymbol.ASSIGN, SyntacticSymbol.EXPRESSION),
    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.ID),
    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.EQ, SyntacticSymbol.EXPRESSION),
    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.IF, SyntacticSymbol.EXPRESSION, SyntacticSymbol.EXPRESSION, SyntacticSymbol.ELSE, SyntacticSymbol.EXPRESSION)
  )

  val (Table(action, goto), init_state) = compute_Table(production__vector)
  println("Action")
  action.foreach(println)
  println("Goto")
  goto.foreach(println)
  println("------")
  val init_status_stack = Vector(init_state)
  val init_node_stack = Vector[Node](BasicNode(""))

  val (node, rest) = LR(Table(action, goto))(init_status_stack, init_node_stack, init_buffer)
  println(node)
}
