package expression

import value._

trait Expression {
    //execute method used to override
    def execute(env:Environment): Value
      
}