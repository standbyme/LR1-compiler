package StandbyMe.compiler

package object universal {

  object SyntacticSymbol extends Enumeration {
    type SyntacticSymbol = Value

    val STARTER, S, A, B, C, D, E, F, M, N, T = Value
    val a, b, c, d, e, ASSIGN, IF, ELSE, $ = Value

    val PRINTLN, INT, INT_KEYWORD, FUNCTION_KEYWORD, FOR_KEYWORD, ID, PLUS, PLUSPLUS, PLUSASSIGN, MINUS, MULTI = Value
    val EXPRESSION, FUNCTION, FUNCTIONS, STATEMENT, STATEMENTS, BLOCK, FUNCTION_CALL = Value
    val LE, GE, GT, EQ = Value
    val LR_BRAC, RR_BRAC, L_BRAC, R_BRAC, SEMIC, COMMA = Value

    def isNonTerminal: SyntacticSymbol => Boolean = Set(FUNCTION_CALL, FUNCTION, FUNCTIONS, STATEMENT, STATEMENTS, BLOCK, EXPRESSION, STARTER, S, A, B, C, D, E, F, M, N, T).contains

    def isTerminal: SyntacticSymbol => Boolean = Set(PRINTLN, FOR_KEYWORD, FUNCTION_KEYWORD, INT_KEYWORD, LE, GE, GT, EQ, PLUSASSIGN, PLUSPLUS, ASSIGN, IF, ELSE, PLUS, MINUS, MULTI, INT, ID, a, b, c, d, e, LR_BRAC, RR_BRAC, SEMIC, L_BRAC, R_BRAC, $).contains

    val V: Set[Value] = values.filter(isNonTerminal)
  }

  type Token = (SyntacticSymbol.SyntacticSymbol, String)

  trait Node {
    def exec(): Any
  }

  trait ExprNode extends Node

  //  case class BinaryOpNode(value: String, left: ExprNode, right: ExprNode) extends ExprNode {
  //    override def toString: String = s"$left $value $right"
  //  }
  //
  //  case class UnaryOpNode(value: String, left: BasicNode) extends ExprNode {
  //    override def toString: String = s"$value $left"
  //  }

  case class IDNode(name: String) extends ExprNode {
    override def toString: String = name

    def exec() = Unit
  }

  case class PrintlnNode(value:ExprNode) extends ExprNode {
    override def toString: String = s"println $value"

    def exec() = println(value.exec())
  }
  case class AssignNode(left: IDNode, right: ExprNode) extends ExprNode {
    override def toString: String = s"$left = $right"

    def exec() = Unit
  }

  case class InitNode(TYPE: String, left: BasicNode, right: ExprNode) extends ExprNode {
    override def toString: String = s"$left init to $TYPE type and assigned value $right"

    def exec() = Unit
  }

  case class IFNode(condition: ExprNode, thenExpr: ExprNode, elseExpr: ExprNode) extends ExprNode {
    override def toString: String = s"if($condition)\n{\n$thenExpr\n} else { \n$elseExpr\n}"

    def exec() = Unit
  }

  case class FORNode(init: ExprNode, condition: ExprNode, action: ExprNode, thenExpr: ExprNode) extends ExprNode {
    override def toString: String = s"for($init ; $condition ; $action)\n{\n$thenExpr\n}"

    def exec() = Unit
  }


  trait LiteralNode extends ExprNode

  case class IntegerLiteralNode(value: Int) extends LiteralNode {
    override def toString: String = value.toString

    def exec() = value
  }

  case class BasicNode(value: String) extends Node {
    override def toString: String = value.toString

    def exec() = Unit
  }

  case class FunctionCallResultNode(functionCallNode: FunctionCallNode) extends ExprNode {
    override def toString: String = functionCallNode.toString

    def exec() = functionCallNode.exec()
  }

  case class BlockNode(statementsNode: StatementsNode) extends Node {
    override def toString: String = s"{$statementsNode}"

    def exec() = statementsNode.exec()
  }

  case class FunctionNode(idNode: IDNode, blockNode: BlockNode) extends Node {
    override def toString: String = s"(call function $idNode ($blockNode))"

    def exec() = blockNode.exec()
  }

  case class FunctionCallNode(idNode: IDNode) extends Node {
    override def toString: String = idNode.toString

    def exec() = {
    }
  }

  case class FunctionsNode(functionNode: FunctionNode, otherNodeOpt: Option[FunctionsNode]) extends Node {
    override def toString: String = otherNodeOpt match {
      case Some(otherNode) => s"$functionNode $otherNode"
      case None => s"$functionNode"
    }

    def exec() = {
      functionNode.exec()
      otherNodeOpt match {
        case Some(otherNode) => otherNode.exec()
        case None => {}
      }
    }
  }

  case class StatementNode(exprNode: ExprNode) extends Node {
    override def toString: String = exprNode.toString

    def exec() = exprNode.exec()

  }

  case class StatementsNode(statementNode: StatementNode, otherNodeOpt: Option[StatementsNode]) extends Node {
    override def toString: String = otherNodeOpt match {
      case Some(otherNode) => s"$statementNode $otherNode"
      case None => s"$statementNode"
    }

    def exec() = {
      statementNode.exec()
      otherNodeOpt match {
        case Some(otherNode) => otherNode.exec()
        case None => {}
      }
    }

  }

}
