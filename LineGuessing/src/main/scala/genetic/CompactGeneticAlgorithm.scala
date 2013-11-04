package genetic

import Genetic._


import Functions._


object CompactGeneticAlgorithm {
  def CGA(F: IndEvalFunction, individualSize: Int, steps: Int, theta: Double): Individual = {

    def CGAAux(n: Int, p: VectorP, x1: Individual, x2: Individual): Individual = {
      val (best, worst) = orderIndividuals(F)(x1, x2)
      println("n=" ++ n.toString
           ++ "  best: " ++ tangensifyIndividual(best).toString
           ++ "  worst: " ++ tangensifyIndividual(worst).toString)
      if (n == 0) best
      else {
        val pbw = p.zip(best.zip(worst))
        val newP = pbw.map({
          case (pi, (1, 0)) => pi + theta
          case (pi, (0, 1)) => pi - theta
          case (pi, _) => pi
        })
        val newX1 = randomIndividual(newP)
        val newX2 = randomIndividual(newP)
        CGAAux(n - 1, newP, newX1, newX2)
      }
    }

    val p = initVectorP(individualSize)
    val x1 = randomIndividual(p)
    val x2 = randomIndividual(p)
    CGAAux(steps, p, x1, x2)
  }
}
