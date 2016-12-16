package expression

import value._
import ui._

case class Disjunction(exp: List[Expression])  extends SpecialForm{
    def execute(env: Environment): Value = {
      var check = false
      
      for (i <- 0 until exp.length-1) {
        
        //check if it is an instance of Boole
        if (exp(i).execute(env).isInstanceOf[Boole]) {
          check = true
        }
        else {
          throw new JediException("Input must be an instance of Boole")
        }
      }
      new Boole(check)
    }
      
      
}