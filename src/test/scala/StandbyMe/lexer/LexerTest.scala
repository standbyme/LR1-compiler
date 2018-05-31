package StandbyMe.lexer

import StandbyMe.compiler.universal.SyntacticSymbol._
import org.scalatest.FunSuite

class LexerTest extends FunSuite {
  test("skip blank") {
    val result = Lexer.skip_blank(" \t\n\r12\t4;\n".toList)
    assert(result == "12\t4;\n".toList)
  }

  test("apply") {
    val code: String = "if 12>5\n6\t \nelse 10;"
    val result = Lexer(code)
    val expected = List((IF, null), (INT, "12"), (GT, null), (INT, "5"), (INT, "6"), (ELSE, null), (INT, "10"), (SEMIC, null))
    assert(result == expected)
  }
}
