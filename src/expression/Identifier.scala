package expression
import value._
import ui._

import ui.UndefinedException
import javax.management.Notification
case class Identifier(name: String) extends Expression with Serializable{
  def execute( env : Environment):Value = {
    //check to see the find
    if (env.find(this) == value.Notification.UNKNOWN ) throw new UndefinedException("Undefined Identifier: "+name)
    
    else env.find(this)
  }
}

