package expression

import value._
import ui._
import javax.management.Notification


case class Conjunction(exp: List[Expression]) extends SpecialForm {
    
    def execute(env: Environment): Value = {
        var result = true
        //analyzes the result and determines if it is true or false
        var i = 0
        
        for (i <- 0 until exp.length - 1) {
          //short circuit evaluation
          if (exp(i).execute(env).isInstanceOf[Boole]) {
            result = exp(i).execute(env).asInstanceOf[Boole].value
          }
          else {
            throw new JediException("Input must be instance of Boole")
          }
        }
        
        new Boole(result)
    }
    
      
       
      
     
}