
import genetic.Genetic.printIndividual
import genetic.Functions._
import genetic.CompactGeneticAlgorithm.CGA
import genetic.PopulationBasedIncrementalLearning.PBIL


object Main extends App {

  println("=============== CGA: ")

  CGA(oneMax, 128, 1000, 0.01).foreach { case ((individual, evaluation)) =>
    println(evaluation.toString ++ ": " ++ printIndividual(individual))
  }

  println("=============== PBIL: ")

  PBIL(oneMax, 100, 128, 1000, 0.1, 0.5, 0.01).foreach { case ((individual, evaluation)) =>
    println(evaluation.toString ++ ": " ++ printIndividual(individual))
  }

}
