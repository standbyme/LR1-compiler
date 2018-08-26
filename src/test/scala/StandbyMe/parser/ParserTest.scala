package StandbyMe.parser

import StandbyMe.compiler.universal.SyntacticSymbol._
import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("Resolve") {
    assert(resolve((STARTER, Vector(), Vector(S), $)) === Some(MatchResult(STARTER, Vector(), S, Vector(), $)))
    assert(resolve((S, Vector(), Vector(C, C), $)) === Some(MatchResult(S, Vector(), C, Vector(C), $)))
    assert(resolve((C, Vector(), Vector(c, C), c)).isEmpty)
    assert(resolve((C, Vector(), Vector(d), c)).isEmpty)
  }
  test("Handle transmit pair") {
    val result = Map(
      A -> Set(b, c)
    )
    val row = A -> Set(M, N)
    assert(transmit_pair_handle(result)(row) == Set((M, Set(b, c)), (N, Set(b, c))))
  }
  test("produce_transmit_map") {
    val production__set = Set(
      A -> Vector(b, c),
      A -> Vector(c),
      B -> Vector(b)
    )
    assert(produce_transmit_map(production__set) == Map(c -> Set(A), b -> Set(A, B)))
  }
  test("compute First") {
    val production__set = Set(
      E -> Vector(T),
      T -> Vector(F),
      T -> Vector(a),
      F -> Vector(LR_BRAC, E, RR_BRAC),
      F -> Vector(b)
    )
    assert(compute_First(production__set).filter(p => isNonTerminal(p._1)) == Map(T -> Set(b, LR_BRAC, a), E -> Set(b, LR_BRAC, a), F -> Set(b, LR_BRAC)))
  }

  test("items") {
    val productionSet = Set(
      STARTER -> Vector(EXPRESSION),
      EXPRESSION -> Vector(C, C),
      C -> Vector(c, C),
      C -> Vector(d)
    )
    val result = items(productionSet)
    val expected = Set(
      Set((C, Vector(), Vector(d), d), (C, Vector(), Vector(d), c), (EXPRESSION, Vector(), Vector(C, C), $), (C, Vector(), Vector(c, C), d), (C, Vector(), Vector(c, C), c), (STARTER, Vector(), Vector(EXPRESSION), $)), // 0
      Set((STARTER, Vector(EXPRESSION), Vector(), $)), // 1
      Set((EXPRESSION, Vector(C), Vector(C), $), (C, Vector(), Vector(c, C), $), (C, Vector(), Vector(d), $)), // 2
      Set((C, Vector(), Vector(d), d), (C, Vector(), Vector(d), c), (C, Vector(), Vector(c, C), d), (C, Vector(), Vector(c, C), c), (C, Vector(c), Vector(C), c), (C, Vector(c), Vector(C), d)), // 3
      Set((C, Vector(d), Vector(), d), (C, Vector(d), Vector(), c)), // 4
      Set((EXPRESSION, Vector(C, C), Vector(), $)), // 5
      Set((C, Vector(c), Vector(C), $), (C, Vector(), Vector(c, C), $), (C, Vector(), Vector(d), $)), // 6
      Set((C, Vector(d), Vector(), $)), // 7
      Set((C, Vector(c, C), Vector(), d), (C, Vector(c, C), Vector(), c)), // 8
      Set((C, Vector(c, C), Vector(), $)) // 9
    )
    assertResult(expected)(result)
  }

  //  test("compute_Table") {
  //    val productionVector = Vector(
  //      Starter -> Vector(S),
  //      S -> Vector(C, C),
  //      C -> Vector(c, C),
  //      C -> Vector(d)
  //    )
  //    val (Table(action, goto),state) = compute_Table(productionVector)
  //    val expected_action = Map(
  //      ((6, d), Action.S(3)),
  //      ((5, c), Action.S(2)),
  //      ((3, c), Action.r(3)),
  //      ((2, c), Action.S(2)),
  //      ((0, d), Action.S(3)),
  //      ((8, $), Action.r(2)),
  //      ((6, c), Action.S(6)),
  //      ((4, c), Action.r(2)),
  //      ((7, $), Action.r(1)),
  //      ((0, c), Action.S(6)),
  //      ((2, d), Action.S(1)),
  //      ((9, $), Action.acc()),
  //      ((4, d), Action.r(2)),
  //      ((1, $), Action.r(3)),
  //      ((3, d), Action.r(3)),
  //      ((5, d), Action.S(1))
  //    )
  //    assertResult(expected_action)(action)
  //    val expected_goto = Map(
  //      ((6, C), 4),
  //      ((0, C), 5),
  //      ((5, C), 7),
  //      ((2, C), 8),
  //      ((0, S), 9)
  //    )
  //    assertResult(expected_goto)(goto)
  //  }
}
