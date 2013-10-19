package genetic

import genetic.Genetic._


object PopulationBasedIncrementalLearning {

  def PBIL(F: IndEvalFunction,
           populationSize: Int,
           individualSize: Int,
           steps: Int,
           th1: Double,
           th2: Double,
           th3: Double, rnd: scala.util.Random): List[(Individual, Int)] = {

    def PBILAux(n: Int, p: VectorP, pop: Population, acc: List[(Individual, Int)]): List[(Individual, Int)] = {
      if (n == 0) acc.reverse
      else {
        val best = bestIndividual(F, pop)
        val px = p.zip(best)
        val newP: VectorP = px.map({ case ((pk, xk)) =>
          val newPK = pk * (1.0 - th1) + xk * th1
          if (uniformRandom(rnd) < th2) newPK * (1.0 - th3) + binaryRandom(0.5) * th3
          else newPK
        })

        val newPopulation = randomPopulation(p, populationSize, rnd)
        PBILAux(n - 1, newP, newPopulation, (best, F(best)) :: acc)
      }
    }

    val p = initVectorP(individualSize)
    PBILAux(steps, p, randomPopulation(p, populationSize), Nil)
  }

}
