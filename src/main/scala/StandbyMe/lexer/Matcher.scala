package StandbyMe.lexer

import StandbyMe.compiler.universal.SyntacticSymbol._
import StandbyMe.compiler.universal.Token

import scala.annotation.tailrec

object Matcher {
  type MatchResult = Option[(Token, List[Char])]

  def keyword_matcher(char__list: List[Char]): MatchResult = char__list match {
    case 'p' :: 'r' :: 'i' :: 'n' :: 't' :: 'l' :: 'n' :: rest_char__list => Some(((PRINTLN, null), rest_char__list))
    case 'i' :: 'f' :: rest_char__list => Some(((IF, null), rest_char__list))
    case 'f' :: 'u' :: 'n' :: 'c' :: 't' :: 'i' :: 'o' :: 'n' :: rest_char__list => Some(((FUNCTION_KEYWORD, null), rest_char__list))
    case 'f' :: 'o' :: 'r' :: rest_char__list => Some(((FOR_KEYWORD, null), rest_char__list))
    case 'i' :: 'n' :: 't' :: rest_char__list => Some(((INT_KEYWORD, null), rest_char__list))
    case 'e' :: 'l' :: 's' :: 'e' :: rest_char__list => Some(((ELSE, null), rest_char__list))
    case _ => None
  }

  def operator_matcher(char__list: List[Char]): MatchResult = char__list match {
    case '>' :: '=' :: rest_char__list => Some(((GE, null), rest_char__list))
    case '<' :: '=' :: rest_char__list => Some(((LE, null), rest_char__list))
    case '>' :: rest_char__list => Some(((GT, null), rest_char__list))
    case '+' :: '+' :: rest_char__list => Some(((PLUSPLUS, null), rest_char__list))
    case '+' :: '=' :: rest_char__list => Some(((PLUSASSIGN, null), rest_char__list))
    case '+' :: rest_char__list => Some(((PLUS, null), rest_char__list))
    case '-' :: rest_char__list => Some(((MINUS, null), rest_char__list))
    case '*' :: rest_char__list => Some(((MULTI, null), rest_char__list))
    case '=' :: '=' :: rest_char__list => Some(((EQ, null), rest_char__list))
    case '=' :: rest_char__list => Some(((ASSIGN, null), rest_char__list))
    case _ => None
  }

  def separator_matcher(char__list: List[Char]): MatchResult = char__list match {
    case '(' :: rest_char__list => Some(((LR_BRAC, null), rest_char__list))
    case ')' :: rest_char__list => Some(((RR_BRAC, null), rest_char__list))
    case '{' :: rest_char__list => Some(((L_BRAC, null), rest_char__list))
    case '}' :: rest_char__list => Some(((R_BRAC, null), rest_char__list))
    case ';' :: rest_char__list => Some(((SEMIC, null), rest_char__list))
    case ',' :: rest_char__list => Some(((COMMA, null), rest_char__list))
    case _ => None
  }

  def integer_constant_matcher(char__list: List[Char]): MatchResult = {
    val (identified_char__list, rest_char__list) = char__list.span(_.isDigit)
    identified_char__list match {
      case Nil => None
      case _ => Some((INT, identified_char__list.mkString("")), rest_char__list)
    }
  }

  def is_valid_char_in_identifier(char: Char): Boolean = char.isLetter || char == '_'

  /**
    * the prefix of ID can't be keyword
    */

  def identifier_matcher(char__list: List[Char]): MatchResult = {
    val (identified_char__list, rest_char__list) = char__list.span(is_valid_char_in_identifier)
    identified_char__list match {
      case Nil => None
      case _ => Some((ID, identified_char__list.mkString("")), rest_char__list)
    }
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
