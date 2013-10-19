package genetic

import Genetic._

object Functions {
  def oneMax(x: Individual) = x.sum
  def pattern(p: Individual)(x: Individual) =
    p.zip(x).count( { case (a, b) => a == b } )
}
