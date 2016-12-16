package ui

import expression._
import value._

object system {
  
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" => mul(args)
      case "div" => div(args)
      case "sub" => sub(args)
      case "equals" => equal(args)
      case "less" => less(args)
      case "more" => more(args)
      case "not" => not(args)
      case "content" => content(args)
      case "var" => makeVar(args)  
      // mul, sub, div, equals, less, etc.
      case _ => throw new UndefinedException(opcode.name)
    }
  }
  
   private def makeVar(args : List [ Value]) = {
     if (args.isEmpty) throw new TypeException("error")
     new Variable(args.head)
   } // new Variable (argss.head)
   
    private def content( args : List[Value]):Value = {
     if (args.isEmpty) throw new TypeException("error")
      if ( args.head.isInstanceOf[Variable]) args.head.asInstanceOf[Variable].contentOfVariable
      else throw new TypeException
   } // args.head.content
   
  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_+_)
  }
  
  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all subtraction inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_-_)
  }
    
  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all division inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_/_)
  }
  
   private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_*_)
  }
  
  private def not(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("not expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("inputs must be boolean type")
    if (vals.length > 1) throw new TypeException("too many inputs")
    val args2 = vals.map(_.asInstanceOf[Boole])
    args2(0)!
  }
  
  private def equal(vals: List[Value]): Value = {
    var isEq = true
    
    if (vals.isEmpty) throw new TypeException("equal expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    
    for(i<-1 until args2.length) {
      if (args2(0).value != args2(i).value) isEq = false
    }
    
    if (isEq) new Boole(true)
    else new Boole(false)
  }

  private def less(vals: List[Value]): Value = {
    var isLess = true
    
    if (vals.isEmpty) throw new TypeException("less expects 1 input")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("input must be a number")
    val args2 = vals.map(_.asInstanceOf[Number])
    
    for (i<-1 until args2.length) {
      if (args2(i - 1).value >= args2(i).value) {
        isLess = false
      }
    }
    
    if (isLess) {
      new Boole(true)
    } else {
      new Boole(false)
    }
  }
  
  private def more(vals: List[Value]): Value = {
    var isMore = true
    
    if (vals.isEmpty) throw new TypeException("more expects 1 input")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("input must be a number")
    val args2 = vals.map(_.asInstanceOf[Number])
    
    for (i<-1 until args2.length) {
      if (args2(i - 1).value <= args2(i).value) {
        isMore = false
      }
    }
    
    if (isMore) {
      new Boole(true)
    } else {
      new Boole(false)
    }
  }
  
  
  
  def test() {
    val globalEnv = new Environment()
    
    val t = new Boole(true)
    val f = new Boole(false)
    
    val num1 = new Number(50.0)
    val num2 = new Number(60.0)
    
    val ident1 = new Identifier("w")
    val ident2 = new Identifier("x")
    val ident3 = new Identifier("y")
    val ident4 = new Identifier("z")
    
    globalEnv.put(List(ident1, ident2, ident3, ident4), List(num1, num2, t, f))
    println("System test method called. Checking globalEnv variables...")
    println("Expected = 50.0, Actual = " + ident1.execute(globalEnv))
    println("Expected = 60.0, Actual = " + ident2.execute(globalEnv))
    println("Expected = true, Actual = " + ident3.execute(globalEnv))
    println("Expected = false, Actual = " + ident4.execute(globalEnv))
    
    println("Checking add(w, 60) function call...")
    var oper = new Identifier("add")
    var args = List(ident1, num2)
    var func = new FunCall(oper, args)
    println("Expected = 110.0, Actual = " + func.execute(globalEnv))
    
    println("Checking add(w, 60, add(w, 60)) function call...")
    var oper2 = new Identifier("add")
    var args2 = List(ident1, num2, func)
    var func2 = new FunCall(oper2, args2)
    println("Expected = 220.0, Actual = " + func2.execute(globalEnv))
    
    println("Checking sub(w, 60)) function call...")
    var oper3 = new Identifier("sub")
    var args3 = List(ident1, num2)
    var func3 = new FunCall(oper3, args3)
    println("Expected = -10.0, Actual = " + func3.execute(globalEnv))

    println("Checking sub(w, 60, sub(w, 60)) function call...")
    val oper4 = new Identifier("sub")
    var args4 = List(ident1, num2, func3)
    var func4 = new FunCall(oper4, args4)
    println("Expected = 0.0, Actual = " + func4.execute(globalEnv))
    
    println("Checking div(w, 60)) function call...")
    oper = new Identifier("div")
    func = new FunCall(oper, args)
    println("Expected = 0.8333333333333334, Actual = " + func.execute(globalEnv))
    
    println("Checking mul(w, 60)) function call...")
    oper = new Identifier("mul")
    func = new FunCall(oper, args)
    println("Expected = 3000.0, Actual = " + func.execute(globalEnv))
    
    println("Checking equal(w, 60)) function call...")
    oper = new Identifier("equals")
    var funcEq = new FunCall(oper, args)
    println("Expected = false, Actual = " + funcEq.execute(globalEnv))
    
    println("Checking less(w, 60)) function call...")
    oper = new Identifier("less")
    func = new FunCall(oper, args)
    println("Expected = true, Actual = " + func.execute(globalEnv))
    
    println("Checking more(w, 60)) function call...")
    oper = new Identifier("more")
    func = new FunCall(oper, args)
    println("Expected = false, Actual = " + func.execute(globalEnv))
    
    println("Checking not(equal(w, 60)) function call...")
    oper = new Identifier("not")
    args2 = List(funcEq)
    func = new FunCall(oper, args2)
    println("Expected = true, Actual = " + func.execute(globalEnv))
  }
}