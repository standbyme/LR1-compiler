package StandbyMe

import StandbyMe.compiler.universal.SyntacticSymbol.SyntacticSymbol

package object lexer {
  type Token = (SyntacticSymbol, String)
}
