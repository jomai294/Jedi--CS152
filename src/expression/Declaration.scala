package expression

import value._
import ui._

case class Declaration( a : Identifier,  e : Expression) extends SpecialForm {
  //execute method that takes in an expression
  def execute( env : Environment):Value = { env.put(a, e.execute(env)); 
  //Notifies the user that it is ok and is stored
  Notification.OK 
  }
}