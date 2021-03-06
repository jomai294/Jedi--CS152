package ui
import scala.util.parsing.combinator._
import expression._
import value._

class WookieParsers extends RegexParsers {


  def declaration: Parser[Expression] = "def" ~ identifier ~ "=" ~ expression ^^
    {
      case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
    }

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression") 
  
  def conditional: Parser[Conditional] = "if"~"("~expression~")"~expression~ opt("else"~expression)^^
  {
    case "if"~"("~exp1~")"~exp2~None => Conditional(exp1,exp2)
    case "if"~"("~ exp1~")"~exp2~Some("else"~exp3) => Conditional(exp1,exp2,exp3)
  }
  
  def operands: Parser[List[Expression]] = "(" ~> opt(expression~rep("," ~> expression)) <~ ")" ^^
  {
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exp) => e::exp
    case _=>Nil
  }

  def funcall: Parser[Expression] = term ~ opt(operands) ^^
  {
    case foo ~ None => foo
    case foo ~ Some(args) => FunCall(foo,args)
    case foo: Literal => throw new JediException("")
  }

  def inequality: Parser[Expression] = sum ~ opt(("<" | ">") ~ sum) ^^
    {
    case t~None=> t
    case t~Some("<"~s)=>FunCall(Identifier("less"), List(t,s))
    case t~Some(">"~s)=>FunCall(Identifier("more"), List(t,s))
    }

  def sum: Parser[Expression] =
    product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
      case p ~ Nil => p
      case p ~ rest => FunCall(Identifier("add"), p :: rest)
    }
  
  def product: Parser[Expression] =
    funcall ~ rep(("/" | "*") ~ funcall ^^ { case "*" ~ s => s case "/" ~ s => invert(s) }) ^^ {
      case p ~ Nil => p
      case p ~ rest => FunCall(Identifier("mul"), p :: rest)
    }

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = new Number(0)
    FunCall(sub, List(zero, exp))
  }

  def reciprocal(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = new Number(1)
    FunCall(div,List(one,exp))
  }
  
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^
    {
      case exp ~ Nil => exp
      case exp ~ expList => FunCall(Identifier("equals"), exp :: expList)
    }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^
    {
      case exp ~ Nil => exp
      case exp ~ expList => Conjunction(exp :: expList)
    }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^
    {
      case exp ~ Nil => exp
      case exp ~ expList => Disjunction(exp :: expList)
    }

  def term: Parser[Expression] =  lambda | block | literal | identifier | "(" ~> expression <~ ")"

  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^
    {
      case e => Identifier(e)
    }

  def literal: Parser[Literal] = boole | numeral

  def boole: Parser[Boole] = """true|false""".r ^^
    {
      case e => new Boole(e.toBoolean)
    }

  def numeral: Parser[Number] = """(/+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case e => new Number(e.toDouble)
    }
  
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~exp => Lambda(params , exp)
  }

  def parameters: Parser[List[Identifier]] =  "(" ~> opt(identifier ~ rep(","~>identifier))<~")" ^^
{
  case None => Nil 
  case Some(e ~ Nil) => List(e) 
  case Some(e ~ exps) => e::exps 
  case _ => Nil
}


 def block: Parser[Expression] = "{" ~ expression ~ rep( ";" ~> expression) ~ "}" ^^{
   case "{" ~ exp ~explist ~"}" => Block(exp :: explist)
   case "{" ~ exp ~ Nil ~ "}" => Block(List(exp))
 }
 
 def invert(exp: Expression): Expression = {
   val div = Identifier("div")
   val one = new Number(1)
   FunCall(div,List(one,exp))
 }
 
 
 
}