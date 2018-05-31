package StandbyMe.parser

import StandbyMe.compiler.universal.SyntacticSymbol.SyntacticSymbol

case class MatchResult(A: SyntacticSymbol, α: Vector[SyntacticSymbol], B: SyntacticSymbol, β: Vector[SyntacticSymbol], a: SyntacticSymbol)
