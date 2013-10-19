
import genetic.Genetic._
import genetic.Functions._
import genetic.CompactGeneticAlgorithm.CGA
import genetic.PopulationBasedIncrementalLearning.PBIL

import scalax.chart._
import scalax.chart.Charting._
import scalax.chart.views._


object Main {

  def showResult(result: List[(Individual, Int)]) = {
    var i = 0
    result.foreach { case ((individual, evaluation)) =>
      i += 1
      println(i.toString ++ ": " ++ evaluation.toString ++ "|" ++ printIndividual(individual))
    }
  }

  def showChart(result: List[(Individual, Int)]) = {
    var i = 0
    val values: Seq[(Int,Int)] = result.map({ case ((individual, evaluation)) => i += 1; (i, evaluation) }).toSeq
    val chart = XYLineChart(values.toXYSeriesCollection(), title = "Convergence of genetic algorithm")
    chart.show(scrollable = true)
  }

  def main(args: Array[String]) = {
    println("Running genetic algorithm...")
    //val result = CGA(oneMax, 1024, 15000, 0.004)
    val result = PBIL(oneMax, 512, 1024, 10000, 0.001, 0.3, 0.1)

    println("Algorithm finished. Displaying chart...")
    showChart(result)

    showResult(result)
  }

}
