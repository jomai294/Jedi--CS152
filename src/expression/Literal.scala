package expression
import value._
case class Literal() extends Expression with Value with Serializable{
  //override the execute method using this method
  def execute( env : Environment):Value = this
}