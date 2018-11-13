package StandbyMe.compiler

import StandbyMe.compiler.universal.{BasicNode, Node, SyntacticSymbol, Token}
import StandbyMe.compiler.universal.SyntacticSymbol._
import StandbyMe.parser.{Production, Table, compute_Table, LR}
import StandbyMe.lexer.Lexer
import scala.io.Source

object Run extends App {
  val production__vector = Vector[Production](
    STARTER -> Vector(FUNCTIONS),
    FUNCTIONS -> Vector(FUNCTION),
    FUNCTIONS -> Vector(FUNCTION, FUNCTIONS),
    FUNCTION -> Vector(FUNCTION_KEYWORD, ID, LR_BRAC, RR_BRAC, BLOCK),
    BLOCK -> Vector(L_BRAC, STATEMENTS, R_BRAC),
    STATEMENTS -> Vector(STATEMENT),
    STATEMENTS -> Vector(STATEMENT, STATEMENTS),
    STATEMENT -> Vector(EXPRESSION, SEMIC),
    EXPRESSION -> Vector(INT),
    FUNCTION_CALL -> Vector(ID, LR_BRAC, RR_BRAC),
    FUNCTION_CALL -> Vector(ID, LR_BRAC, EXPRESSION, RR_BRAC),
    EXPRESSION -> Vector(FUNCTION_CALL),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.INT_KEYWORD, SyntacticSymbol.ID, SyntacticSymbol.ASSIGN, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.FOR_KEYWORD, SyntacticSymbol.LR_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.RR_BRAC, SyntacticSymbol.L_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.R_BRAC),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.PLUS, SyntacticSymbol.EXPRESSION),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.ID, SyntacticSymbol.ASSIGN, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC),
    //
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.ID),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.EQ, SyntacticSymbol.EXPRESSION),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.LE, SyntacticSymbol.EXPRESSION),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.ID, SyntacticSymbol.PLUSPLUS),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.ID, SyntacticSymbol.PLUSASSIGN, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC),
    //    SyntacticSymbol.EXPRESSION -> Vector(SyntacticSymbol.IF, SyntacticSymbol.LR_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.RR_BRAC, SyntacticSymbol.L_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.R_BRAC, SyntacticSymbol.ELSE, SyntacticSymbol.L_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.R_BRAC)
  )

  val (Table(action, goto), init_state) = compute_Table(production__vector)
  println("Action")
  action.foreach(println)
  println("Goto")
  goto.foreach(println)
  println("------")

  val code: String = Source.fromFile("data").mkString

  val result = Lexer(code).toVector
  result.foreach(println)
  println("------")
  val init_buffer: Vector[Token] = result ++ Vector((SyntacticSymbol.$, null))

  val init_status_stack = Vector(init_state)
  val init_node_stack = Vector[Node](BasicNode(""))

  val (node, rest) = LR(Table(action, goto))(init_status_stack, init_node_stack, init_buffer)
  println(node)

}
