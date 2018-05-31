package StandbyMe.parser

import StandbyMe.compiler.universal.SyntacticSymbol._
import org.scalatest.FunSuite

class ClosureUtilTest extends FunSuite {
  val productionSet = Set(
    Starter -> Vector(S),
    S -> Vector(C, C),
    C -> Vector(c, C),
    C -> Vector(d)
  )
  val CLOSURE = new ClosureUtil(productionSet)


  test("Single Step") {

    val item = (Starter, Vector(), Vector(S), $)
    val result = CLOSURE.single_step(item)
    assert(result == Set((S, Vector(), Vector(C, C), $)))
  }

  test("Closure") {
    val item = (Starter, Vector(), Vector(S), $)
    val result = CLOSURE(item)
    val expected = Set(
      (Starter, Vector(), Vector(S), $),
      (S, Vector(), Vector(C, C), $),
      (C, Vector(), Vector(d), d),
      (C, Vector(), Vector(d), c),
      (C, Vector(), Vector(c, C), d),
      (C, Vector(), Vector(c, C), c)
    )
    assert(result == expected)
  }

}

