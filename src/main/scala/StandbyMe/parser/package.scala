package StandbyMe

import StandbyMe.compiler.universal.{ExprNode, _}
import StandbyMe.compiler.universal.SyntacticSymbol.{SyntacticSymbol, isNonTerminal, isTerminal}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

package object parser {

  type SS = SyntacticSymbol
  type Production = (SS, Vector[SS])
  type ProductionSet = Set[Production]
  type Item = (SS, Vector[SS], Vector[SS], SS)
  type First = HashMap[SS, Set[SS]]
  type Closure = Set[Item]
  type ItemSet = Set[Item]
  type State = Int

  case class Table(action: Map[(State, SS), Action.Action], goto: Map[(State, SS), State])

  def resolve(item: Item): Option[MatchResult] = item match {
    case (aA, α, bB +: β, a) if isNonTerminal(bB) && isTerminal(a) => Some(MatchResult(aA, α, bB, β, a))
    case _ => None
  }

  def transmit_pair_handle(result: Map[SS, Set[SS]])(row: (SS, Set[SS])): Set[(SS, Set[SS])] = {
    val (one, two) = row
    two.map((_, result(one)))
  }

  def produce_transmit_map(production__set: Set[Production]): HashMap[SS, Set[SS]] = {

    val collection = production__set
      .map { case (one, two) => (two.head, one) }
      .filter { case (one, two) => one != two }
      .groupBy(_._1)
      .map { case (m, n) => (m, n.map(_._2)) }
      .toVector
    HashMap(collection: _*)
  }

  @tailrec
  def compute_First_helper(result: HashMap[SS, Set[SS]], transmit_map: HashMap[SS, Set[SS]]): HashMap[SS, Set[SS]] = {
    if (transmit_map.isEmpty) result
    else {
      val (success, failure) = transmit_map
        .partition { case (one, _) => result.contains(one) && transmit_map.values.forall(!_.contains(one)) }

      val next_result_mid = success
        .toSet
        .flatMap(transmit_pair_handle(result))
        .groupBy(_._1)
        .map { case (one, two) => (one, two.flatMap(_._2)) }
        .toList

      val next_result = HashMap(next_result_mid: _*).merged(result) { case ((k, v1), (_, v2)) => (k, v1 ++ v2) }
      val next_transmit_map = failure
      compute_First_helper(next_result, next_transmit_map)
    }
  }

  def make_pair(syntacticSymbol: SS): (SS, Set[SS]) = (syntacticSymbol, Set(syntacticSymbol))


  def compute_First(productionSet: ProductionSet): HashMap[SS, Set[SS]] = {

    val collection = SyntacticSymbol
      .values
      .filter(isTerminal)
      .map(make_pair)
      .toVector
    val init_result: HashMap[SS, Set[SS]] = HashMap(collection: _*)
    val init_transmit_map = produce_transmit_map(productionSet)

    compute_First_helper(init_result, init_transmit_map)
  }

  def GotoUtil(CLOSURE: ClosureUtil)(I: ItemSet, X: SS): ItemSet = {
    def item_handler(item: Item): Option[Item] = item match {
      case (aA, α, bB +: β, a) if bB == X => Some((aA, α :+ bB, β, a))
      case _ => None
    }

    I.flatMap(item_handler).flatMap(CLOSURE(_))
  }

  def items(productionSet: ProductionSet): Set[Closure] = {
    val CLOSURE = new ClosureUtil(productionSet)
    val GOTO: Function2[ItemSet, SS, ItemSet] = GotoUtil(CLOSURE)

    def helper(workbench: Set[Closure])(result: Set[Closure]): Set[Closure] = {
      if (workbench.isEmpty) result
      else {
        val next_result = workbench ++ result
        val next_workbench_mid = for {
          iI <- workbench
          xX <- SyntacticSymbol.values
          g = GOTO(iI, xX)
          if g.nonEmpty
        } yield g
        val next_workbench = next_workbench_mid.diff(next_result)
        helper(next_workbench)(next_result)
      }
    }

    val item = (SyntacticSymbol.STARTER, Vector(), Vector(SyntacticSymbol.FUNCTIONS), SyntacticSymbol.$)
    val C = Set(CLOSURE(item))
    helper(C)(Set())
  }

  def compute_Table(productionVector: Vector[Production]): (Table, State) = {
    val productionSet = productionVector.toSet
    val production_index_map = productionVector.zipWithIndex.toMap
    val canonicalCollection: Vector[Closure] = items(productionSet).toVector
    val closure_index_map = canonicalCollection.zipWithIndex.toMap
    val CLOSURE = new ClosureUtil(productionSet)
    val init_state = closure_index_map(CLOSURE((SyntacticSymbol.STARTER, Vector(), Vector(SyntacticSymbol.FUNCTIONS), SyntacticSymbol.$)))
    val GOTO: (ItemSet, SS) => ItemSet = GotoUtil(CLOSURE)
    val init_action = Map[(State, SS), Action.Action]()
    val init_goto = Map[(State, SS), State]()
    val init_table = Table(init_action, init_goto)

    def outer(table: Table, I: Closure): Table = {
      val k = closure_index_map(I)
      val Table(action, goto) = table

      def inner(action: Map[(State, SS), Action.Action], item: Item): Map[(State, SS), Action.Action] = {
        item match {
          case (SyntacticSymbol.STARTER, Vector(SyntacticSymbol.FUNCTIONS), Vector(), SyntacticSymbol.$) => action + ((k, SyntacticSymbol.$) -> Action.acc())
          case (aA, α, a +: β, b) if isTerminal(a) =>
            val j = closure_index_map(GOTO(I, a))
            action + ((k, a) -> Action.S(j))
          case (aA, α, Vector(), a) =>
            val production = aA -> α
            action + ((k, a) -> Action.r(production))
          case _ => action
        }
      }

      val next_acion = I.foldLeft(action)(inner)
      val next_goto = goto ++ SyntacticSymbol.V.flatMap(B => closure_index_map.get(GOTO(I, B)).map(j => (k, B) -> j))
      Table(next_acion, next_goto)
    }

    (canonicalCollection.foldLeft(init_table)(outer), init_state)
  }

  def reduce(production: Production, right: Vector[Node]): Node = {
    production match {
      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.INT)) => IntegerLiteralNode(right(0).asInstanceOf[BasicNode].value.toInt)
      case (SyntacticSymbol.STATEMENT, Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC)) => StatementNode(right(1).asInstanceOf[ExprNode])
      case (SyntacticSymbol.STATEMENTS, Vector(SyntacticSymbol.STATEMENT)) => StatementsNode(right(0).asInstanceOf[StatementNode],None)
      case (SyntacticSymbol.BLOCK, Vector(SyntacticSymbol.L_BRAC,SyntacticSymbol.STATEMENTS,SyntacticSymbol.R_BRAC)) => BlockNode(right(1).asInstanceOf[StatementsNode])
      case (SyntacticSymbol.FUNCTION, Vector(SyntacticSymbol.FUNCTION_KEYWORD, SyntacticSymbol.ID, SyntacticSymbol.LR_BRAC, SyntacticSymbol.RR_BRAC, SyntacticSymbol.BLOCK)) => FunctionNode(right(0).asInstanceOf[BlockNode])
      case (SyntacticSymbol.FUNCTIONS, Vector(SyntacticSymbol.FUNCTION)) => FunctionsNode(right(0).asInstanceOf[FunctionNode],None)


      //      case (SyntacticSymbol.STARTER, Vector(SyntacticSymbol.EXPRESSION)) => right(0).asInstanceOf[ExprNode]
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.INT_KEYWORD, SyntacticSymbol.ID, SyntacticSymbol.ASSIGN, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC)) => InitNode("int", right(3).asInstanceOf[BasicNode], right(1).asInstanceOf[ExprNode])
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.FOR_KEYWORD, SyntacticSymbol.LR_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.RR_BRAC, SyntacticSymbol.L_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.R_BRAC)) => FORNode(right(7).asInstanceOf[ExprNode], right(6).asInstanceOf[ExprNode], right(4).asInstanceOf[ExprNode], right(1).asInstanceOf[ExprNode])
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.LE, SyntacticSymbol.EXPRESSION)) => BinaryOpNode("<=", right(0).asInstanceOf[ExprNode], right(2).asInstanceOf[ExprNode])
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.INT)) => IntegerLiteralNode(right(0).asInstanceOf[BasicNode].value.toInt)
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.PLUS, SyntacticSymbol.EXPRESSION)) => BinaryOpNode("+", right(0).asInstanceOf[ExprNode], right(2).asInstanceOf[ExprNode])
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.ID, SyntacticSymbol.PLUSPLUS)) => UnaryOpNode("++", right(1).asInstanceOf[BasicNode])
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.ID, SyntacticSymbol.PLUSASSIGN, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC)) => {
      //        BinaryOpNode("+=", IDNode(right(3).asInstanceOf[BasicNode].value), right(1).asInstanceOf[ExprNode])
      //      }
      //
      //
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.ID)) => IDNode(right(0).asInstanceOf[BasicNode].value)
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.ID, SyntacticSymbol.ASSIGN, SyntacticSymbol.EXPRESSION, SyntacticSymbol.SEMIC)) => AssignNode(IDNode(right(3).asInstanceOf[BasicNode].value), right(1).asInstanceOf[ExprNode])
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.IF, SyntacticSymbol.LR_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.RR_BRAC, SyntacticSymbol.L_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.R_BRAC, SyntacticSymbol.ELSE, SyntacticSymbol.L_BRAC, SyntacticSymbol.EXPRESSION, SyntacticSymbol.R_BRAC)) =>
      //        IFNode(right(8).asInstanceOf[ExprNode], right(5).asInstanceOf[ExprNode], right(1).asInstanceOf[ExprNode])
      //      case (SyntacticSymbol.EXPRESSION, Vector(SyntacticSymbol.EXPRESSION, SyntacticSymbol.EQ, SyntacticSymbol.EXPRESSION)) => BinaryOpNode("==", right(2).asInstanceOf[ExprNode], right(0).asInstanceOf[ExprNode])

    }
  }

  def LR(table: Table)(status_stack: Vector[State], node_stack: Vector[Node], buffer: Vector[Token]): (Node, Vector[Token]) = {
    val Table(action, goto) = table
    val S = status_stack.head
    val a = buffer.head
    action.get((S, a._1)) match {
      case Some(action_value) => action_value match {
        case Action.S(i) =>
          val node = BasicNode(a._2)
          LR(table: Table)(i +: status_stack, node +: node_stack, buffer.tail)
        case Action.r(production) =>
          val A = production._1
          val β = production._2
          println(production)
          val l = β.length
          val new_status_stack = status_stack.drop(l)
          val new_S = new_status_stack.head
          val j = goto((new_S, A))
          val (right, new_node_stack) = node_stack.splitAt(l)
          val node = reduce(production, right)
          LR(table: Table)(j +: new_status_stack, node +: node_stack.drop(l), buffer)
        case Action.acc() =>
          println("Accept")
          (node_stack(0), buffer)
      }
      case None => {
        println((S, a._1))
        throw new Exception("Error")
      }

    }
  }

}
