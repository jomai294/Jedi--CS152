package expression
import value._
case class Lambda(parameters:List[Identifier], expBody:Expression) extends SpecialForm {
  
 def execute(env : Environment):Value = {
   new Closure(parameters,expBody,env)
   
 }
}