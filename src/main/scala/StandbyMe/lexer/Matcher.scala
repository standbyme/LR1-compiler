package StandbyMe.lexer

import StandbyMe.compiler.universal.SyntacticSymbol._
import StandbyMe.compiler.universal.Token

import scala.annotation.tailrec

object Matcher {
  type MatchResult = Option[(Token, List[Char])]

  def keyword_matcher(char__list: List[Char]): MatchResult = char__list match {
    case 'i' :: 'f' :: rest_char__list => Some(((IF, null), rest_char__list))
    case 'e' :: 'l' :: 's' :: 'e' :: rest_char__list => Some(((ELSE, null), rest_char__list))
    case _ => None
  }

  def operator_matcher(char__list: List[Char]): MatchResult = char__list match {
    case '>' :: '=' :: rest_char__list => Some(((GE, null), rest_char__list))
    case '>' :: rest_char__list => Some(((GT, null), rest_char__list))
    case '+' :: rest_char__list => Some(((PLUS, null), rest_char__list))
    case '-' :: rest_char__list => Some(((MINUS, null), rest_char__list))
    case '*' :: rest_char__list => Some(((MULTI, null), rest_char__list))
    case '=' :: '=' :: rest_char__list => Some(((ASSIGN, null), rest_char__list))
    case '=' :: rest_char__list => Some(((EQ, null), rest_char__list))
    case _ => None
  }

  def separator_matcher(char__list: List[Char]): MatchResult = char__list match {
    case '(' :: rest_char__list => Some(((LR_BRAC, null), rest_char__list))
    case ')' :: rest_char__list => Some(((RR_BRAC, null), rest_char__list))
    case ';' :: rest_char__list => Some(((SEMIC, null), rest_char__list))
    case _ => None
  }

  @tailrec
  def integer_constant_matcher_helper(char__zipper: Zipper[Char]): Option[(List[Char], List[Char])] = {
    val past = char__zipper.past
    val now = char__zipper.now
    val future = char__zipper.future
    if (now.isDigit) char__zipper.forward match {
      case Some(next_char__zipper) => integer_constant_matcher_helper(next_char__zipper)
      case None =>
        assert(future == Nil)
        Some((now :: past, future))
    } else {
      if (past.nonEmpty) Some((past, now :: future))
      else None
    }
  }

  def integer_constant_matcher(char__list: List[Char]): MatchResult = {
    val char__zipper = Zipper(char__list)
    integer_constant_matcher_helper(char__zipper) map { case (identified_char__list, rest_char__list) => ((INT, identified_char__list.reverse.mkString("")), rest_char__list) }
  }

  def is_valid_char_in_identifier(char: Char): Boolean = char.isLetter || char == '_'


  @tailrec
  def identifier_matcher_helper(char__zipper: Zipper[Char]): Option[(List[Char], List[Char])] = {
    val past = char__zipper.past
    val now = char__zipper.now
    val future = char__zipper.future
    if (is_valid_char_in_identifier(now)) char__zipper.forward match {
      case Some(next_char__zipper) => identifier_matcher_helper(next_char__zipper)
      case None =>
        assert(future == Nil)
        Some((now :: past, future))
    } else {
      if (past.nonEmpty) Some((past, now :: future))
      else None
    }
  }

  /**
    * the prefix of ID can't be keyword
    */
  def identifier_matcher(char__list: List[Char]): MatchResult = {
    val char__zipper = Zipper(char__list)
    identifier_matcher_helper(char__zipper) map { case (identified_char__list, rest_char__list) => ((ID, identified_char__list.reverse.mkString("")), rest_char__list) }
  }

  def constant_matcher(char__list: List[Char]): MatchResult = {
    lazy val integer_constant_result = integer_constant_matcher(char__list)
    integer_constant_result
  }

  def apply(char__list: List[Char]): MatchResult = {
    lazy val keyword_result = keyword_matcher(char__list)
    lazy val operator_result = operator_matcher(char__list)
    lazy val separator_result = separator_matcher(char__list)
    lazy val constant_result = constant_matcher(char__list)
    lazy val identifier_result = identifier_matcher(char__list)

    keyword_result
      .orElse(operator_result)
      .orElse(separator_result)
      .orElse(constant_result)
      .orElse(identifier_result)
  }
}
