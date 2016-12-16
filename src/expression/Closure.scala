package expression
import value._
import ui._
class Closure(parameter: List[Identifier], expBody: Expression, defEnv: Environment) extends Value {
   def apply(args: List[Value] , callEnv: Environment = null): Value = {
     var tempEnv = new Environment()
      if(callEnv == null) tempEnv = new Environment(defEnv)
      else tempEnv = callEnv
    
       if (args.length == parameter.length){
       
        tempEnv.put(parameter, args)
        expBody.execute(tempEnv)
     }
     else {
       throw new TypeException
     }
   }
}
