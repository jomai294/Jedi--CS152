package expression

import value._

case class Conditional(expression1: Expression, expression2: Expression, expression3: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value ={
    //check if it is a true condition
    if ( expression1.execute(env).toString() == "true") expression2.execute(env)
    //check if it is a false condition
    else if (expression1.execute(env).toString() == "false") expression3. execute(env)  
    //if it fulfills none, then it is unknown
    else Notification.UNKNOWN 
  }
    
}