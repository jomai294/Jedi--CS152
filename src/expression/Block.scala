package expression

import value._

case class Block(localExps: List[Expression]) extends SpecialForm {
  def execute( env : Environment):Value = 
  {
    var localEnv = new Environment(env)
    for ( i <- 0 until localExps.length-1)
    {
      localExps(i).execute(localEnv)
    }
    localExps(localExps.length-1).execute(localEnv)
  }
}