import scala.util.parsing.combinator._
import scala.io.Source

case class DELIMETER(str: String)
case class OPERATOR(str: String)
case class EXP(cont1: Any, cont2: Any)
case class TERM(t1: Any, t2: Any)
case class FACTOR(content: Any)
case class EXPLIST(list: Any)
case class PROPEXPLIST(exp: Any, list: List[Any])
case class IDLIST(list: Any)
case class PROPIDLIST(id: ID, list: List[ID])
case class DEF(id: Any, exp: Any)
case class BOOL(boolean: String)
case class UNOP(op: OPERATOR)
case class SIGN(op: OPERATOR)
case class BINOP(op: OPERATOR)
case class PRIM(str: String)
case class ID(str: String)
case class INTEGER(str: String)

class SimpleParser extends RegexParsers {

  val characters  = "[a-zA-Z?_]".r
  val words       = "[a-zA-Z?_]+".r
  val digits      = "[0-9]".r
  val numbers     = "[0-9]+".r
  val delimiters  = "(" | ")" | "[" | "]" | "," | ";"
  val signs       = "+" | "-"
  val unops       = "+" | "-" | "~"
  val binops      = "<=" | ">=" | "!=" | "&" | "|" | "+" | "-" | "*" | "/" | "=" | "<" | ">"
  val operators   = "!=" | "<" | ">" | "<=" | ">=" | ":=" |"+" | "-" | "*" | "/" | "=" | "&" | "|"
  val keyWords    = "if" | "then" | "else" | "let" | "in" | "map" | "to"
  val prims       = "number?" | "function?" | "list?" | "empty?" | "cons?" | "cons" | "first" | "rest" | "arity"
  val booleans    = "true" | "false"
  val ignore      = not(reserved | bool)

  def delimiter: Parser[DELIMETER] = delimiters ^^ {
    deli => DELIMETER(deli)
  }

  def operator: Parser[OPERATOR] = operators ^^ {
    operators => OPERATOR(operators)
  }

  def prim: Parser[PRIM] = prims ^^ {
    prim => PRIM(prim)
  }

  def reserved: Parser[String] = keyWords ^^ {
    _.toString
  }

  def word: Parser[String] = words ^^ {
    _.toString
  }

  def digit: Parser[String] = digits ^^ {
    _.toString
  }

  def int: Parser[INTEGER] = numbers ^^ {
    (int => INTEGER(int))
  }

  def bool: Parser[BOOL] = booleans ^^ {
    case b => BOOL(b)
  }

  def exp: Parser[Any] = ((term ~ opt(binop ~ exp)) |  ("if " ~ exp ~ "then " ~ exp ~ "else " ~ exp) | ("let " ~ rep1(definer) ~ "in " ~ exp) | ("map " ~ idList ~ "to " ~ exp)) ^^ {
    case "if " ~ e1 ~ "then " ~ e2 ~ "else" ~ e3 => " if " + e1 + " then " + e2 + " else " + e3
    case "let " ~ list ~ " in " ~ e => " let " + list + " in " + e
    case "map " ~ idList ~ "to " ~ exp => " map " + idList + " to " + exp
    case TERM(t1,t2) ~ Some(e) => TERM(t1, t2) + " " + e
    case TERM(t1,t2) ~ None => EXP(TERM(t1,t2), None)
    case e => e
  }

  def term: Parser[TERM] = ((unop ~ term) | (factor ~ "(" ~ ")") | (factor ~ opt("(" ~> expList <~ ")")) | int | bool) ^^ {
    case BOOL(bool)             => TERM(BOOL(bool), None)
    case INTEGER(int)           => TERM(INTEGER(int), None)
    case UNOP(op) ~ TERM(t1,t2) => TERM(UNOP(op), TERM(t1,t2))
    case FACTOR(f) ~ Some(c)    => TERM(FACTOR(f), c)
    case FACTOR(f) ~ "(" ~ ")"  => TERM(FACTOR(f), ())
    case FACTOR(f) ~ None       => TERM(FACTOR(f), None)
  }

  def factor: Parser[FACTOR] = (("(" ~ exp ~ ")") | prim | id) ^^ {
    case ID(id)     => FACTOR(ID(id))
    case PRIM(prim) => FACTOR(PRIM(prim))
    case exp        => FACTOR(exp)
  }

  def sign: Parser[SIGN] = signs ^^ {
    case operators => SIGN(OPERATOR(operators))
  }

  def unop: Parser[UNOP] = unops ^^ {
    case operators => UNOP(OPERATOR(operators))
  }

  def binop: Parser[BINOP] = binops ^^ {
    case operators => BINOP(OPERATOR(operators))
  }

  def idList: Parser[IDLIST] = propIdList ^^ {
    case list => IDLIST(list)
  }

  def expList: Parser[EXPLIST] = propExpList ^^ {
    case list => EXPLIST(list)
  }

  def propExpList: Parser[Any] = (exp ~ rep("," ~> exp)) ^^ {
    case exp ~ list => PROPEXPLIST(exp, list)
    case exp ~ Nil  => exp
  }
  def propIdList: Parser[Any] = (id ~ rep("," ~> id)) ^^ {
    case id ~ list  => PROPIDLIST(id, list)
    case id ~ Nil   => id
  }

  def definer: Parser[Any] = (id ~ ":=" ~ exp ~ ";") ^^ {
    case id ~ exp => DEF(id, exp)
  }

  def id: Parser[ID] = (ignore ~> word ~ rep(ignore ~> word | digit)) ^^ {
    case p ~ n    => ID(p + n)
    case p ~ Nil  => ID(p)
  }

}

//Test
object Main{
  def main(args : Array[String]) {

    val fileContents = Source.fromFile("Test").getLines.mkString
    val parser = new SimpleParser
    val result = parser.parseAll(parser.exp, fileContents)
    println(result)

  }
}