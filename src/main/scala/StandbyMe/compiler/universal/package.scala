package StandbyMe.compiler

package object universal {

  object SyntacticSymbol extends Enumeration {
    type SyntacticSymbol = Value

    val STARTER, S, A, B, C, D, E, F, M, N, T = Value
    val a, b, c, d, e, ASSIGN, IF, ELSE, $ = Value

    val INT, INT_KEYWORD, FOR_KEYWORD, ID, PLUS, PLUSPLUS, PLUSASSIGN, MINUS, MULTI = Value
    val EXPRESSION = Value
    val LE, GE, GT, EQ = Value
    val LR_BRAC, RR_BRAC, L_BRAC, R_BRAC, SEMIC, COMMA = Value

    def isNonTerminal: SyntacticSymbol => Boolean = Set(EXPRESSION, STARTER, S, A, B, C, D, E, F, M, N, T).contains

    def isTerminal: SyntacticSymbol => Boolean = Set(FOR_KEYWORD, INT_KEYWORD, LE, GE, GT, EQ, PLUSASSIGN, PLUSPLUS, ASSIGN, IF, ELSE, PLUS, MINUS, MULTI, INT, ID, a, b, c, d, e, LR_BRAC, RR_BRAC, SEMIC, L_BRAC, R_BRAC, $).contains

    val V: Set[Value] = values.filter(isNonTerminal)
  }

  type Token = (SyntacticSymbol.SyntacticSymbol, String)

  trait Node

  trait ExprNode extends Node

  case class BinaryOpNode(value: String, left: ExprNode, right: ExprNode) extends ExprNode {
    override def toString: String = s"$left $value $right"
  }

  case class UnaryOpNode(value: String, left: BasicNode) extends ExprNode {
    override def toString: String = s"$value $left"
  }

  case class IDNode(name: String) extends ExprNode {
    override def toString: String = name
  }

  case class AssignNode(left: IDNode, right: ExprNode) extends ExprNode {
    override def toString: String = s"$left = $right"
  }

  case class InitNode(TYPE: String, left: BasicNode, right: ExprNode) extends ExprNode {
    override def toString: String = s"$left init to $TYPE type and assigned value $right"
  }

  case class IFNode(condition: ExprNode, thenExpr: ExprNode, elseExpr: ExprNode) extends ExprNode {
    override def toString: String = s"if($condition)\n{\n$thenExpr\n} else { \n$elseExpr\n}"
  }

  case class FORNode(init: ExprNode, condition: ExprNode, action: ExprNode, thenExpr: ExprNode) extends ExprNode {
    override def toString: String = s"for($init ; $condition ; $action)\n{\n$thenExpr\n}"
  }

  case class CallNode(func: IDNode, para: ExprNode)

  trait LiteralNode extends ExprNode

  case class IntegerLiteralNode(value: Int) extends LiteralNode {
    override def toString: String = value.toString
  }

  case class BasicNode(value: String) extends Node {
    override def toString: String = value.toString
  }

}
