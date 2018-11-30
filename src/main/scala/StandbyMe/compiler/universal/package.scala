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
    def exec(env: Env): ExecResult
  }

  trait ExprNode extends Node

  case class BinaryOpNode(value: String, left: ExprNode, right: ExprNode) extends ExprNode {
    override def toString: String = s"$left $value $right"

    override def exec(env: Env): ExecResult = value match {
      case "+" => ExecResult(IntegerLiteralNode(left.exec(env).value.asInstanceOf[IntegerLiteralNode].value + right.exec(env).value.asInstanceOf[IntegerLiteralNode].value), env)
      case "-" => ExecResult(IntegerLiteralNode(left.exec(env).value.asInstanceOf[IntegerLiteralNode].value - right.exec(env).value.asInstanceOf[IntegerLiteralNode].value), env)
      case "*" => ExecResult(IntegerLiteralNode(left.exec(env).value.asInstanceOf[IntegerLiteralNode].value * right.exec(env).value.asInstanceOf[IntegerLiteralNode].value), env)
    }
  }

  //
  //  case class UnaryOpNode(value: String, left: BasicNode) extends ExprNode {
  //    override def toString: String = s"$value $left"
  //  }

  case class IDNode(name: String) extends ExprNode {
    override def toString: String = name

    override def exec(env: Env): ExecResult = ExecResult(env.get(name).get.asInstanceOf[ExprNode], env)
  }

  case class PrintlnNode(value: ExprNode) extends ExprNode {
    override def toString: String = s"println $value"

    override def exec(env: Env): ExecResult = {
      println(value.exec(env).value)
      ExecResult(IntegerLiteralNode(0), env)
    }
  }

  case class AssignNode(key: String, value: ExprNode) extends ExprNode {
    override def toString: String = s"$key = $value"

    override def exec(env: Env): ExecResult = ExecResult(null, env.set(key, value.exec(env).value))
  }

  //  case class InitNode(TYPE: String, left: BasicNode, right: ExprNode) extends ExprNode {
  //    override def toString: String = s"$left init to $TYPE type and assigned value $right"
  //
  //    def exec() = Unit
  //  }
  //
  case class IFNode(condition: ExprNode, thenStatements: StatementsNode, elseStatements: StatementsNode) extends ExprNode {
    override def toString: String = s"if($condition)\n{\n$thenStatements\n} else { \n$elseStatements\n}"

    override def exec(env: Env): ExecResult = {
      if (condition.exec(env).value.asInstanceOf[IntegerLiteralNode].value != 0) {
        thenStatements.exec(env)
      } else {
        elseStatements.exec(env)
      }
    }
  }

  //  case class FORNode(init: ExprNode, condition: ExprNode, action: ExprNode, thenExpr: ExprNode) extends ExprNode {
  //    override def toString: String = s"for($init ; $condition ; $action)\n{\n$thenExpr\n}"
  //
  //    def exec() = Unit
  //  }


  trait LiteralNode extends ExprNode

  case class IntegerLiteralNode(value: Int) extends LiteralNode {
    override def toString: String = value.toString

    override def exec(env: Env): ExecResult = ExecResult(this, env)
  }

  case class BasicNode(value: String) extends Node {
    override def toString: String = value.toString

    override def exec(env: Env): ExecResult = throw new Exception("BasicNode can't exec")
  }

  case class FunctionNode(name: String, statementsNode: StatementsNode) extends Node {
    override def toString: String = s"(def function $name ($statementsNode))"

    override def exec(env: Env): ExecResult = statementsNode.exec(env)

    //    def exec() = statementsNode.exec()
  }

  case class FunctionCallNode(name: String) extends ExprNode {
    override def toString: String = name

    override def exec(env: Env): ExecResult = env.get(name).get.asInstanceOf[FunctionNode].exec(env)

  }

  case class FunctionsNode(functionNode: FunctionNode, otherNodeOpt: Option[FunctionsNode]) extends Node {
    override def toString: String = otherNodeOpt match {
      case Some(otherNode) => s"$functionNode $otherNode"
      case None => s"$functionNode"
    }

    override def exec(env: Env): ExecResult = null

    def toTable: Map[String, Node] = {
      otherNodeOpt match {
        case Some(otherNode) => otherNode.toTable + (functionNode.name -> functionNode)
        case None => Map(functionNode.name -> functionNode)
      }
    }
  }

  case class StatementNode(exprNode: ExprNode) extends Node {
    override def toString: String = exprNode.toString

    override def exec(env: Env): ExecResult = exprNode.exec(env)
  }

  case class StatementsNode(statementNode: StatementNode, otherNodeOpt: Option[StatementsNode]) extends Node {
    override def toString: String = otherNodeOpt match {
      case Some(otherNode) => s"$statementNode $otherNode"
      case None => s"$statementNode"
    }

    override def exec(env: Env): ExecResult = {
      val execResult = statementNode.exec(env)
      otherNodeOpt match {
        case Some(otherNode) => otherNode.exec(execResult.env)
        case None => execResult
      }
    }
  }

}
