
import genetic.Genetic.printIndividual
import genetic.Functions._
import genetic.CompactGeneticAlgorithm.CGA


object Main extends App {

  CGA(oneMax, 200, 2000, 0.01).foreach { case ((individual, evaluation)) =>
    println(evaluation.toString ++ ": " ++ printIndividual(individual))
  }

}
