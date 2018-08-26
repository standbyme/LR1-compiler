package StandbyMe.parser

import StandbyMe.compiler.universal.SyntacticSymbol._
import org.scalatest.FunSuite

class ClosureUtilTest extends FunSuite {
  val productionSet = Set(
    STARTER -> Vector(S),
    S -> Vector(C, C),
    C -> Vector(c, C),
    C -> Vector(d)
  )
  val CLOSURE = new ClosureUtil(productionSet)


  test("Single Step") {

    val item = (STARTER, Vector(), Vector(S), $)
    val result = CLOSURE.single_step(item)
    assert(result == Set((S, Vector(), Vector(C, C), $)))
  }

  test("Closure") {
    val item = (STARTER, Vector(), Vector(S), $)
    val result = CLOSURE(item)
    val expected = Set(
      (STARTER, Vector(), Vector(S), $),
      (S, Vector(), Vector(C, C), $),
      (C, Vector(), Vector(d), d),
      (C, Vector(), Vector(d), c),
      (C, Vector(), Vector(c, C), d),
      (C, Vector(), Vector(c, C), c)
    )
    assert(result == expected)
  }

}

