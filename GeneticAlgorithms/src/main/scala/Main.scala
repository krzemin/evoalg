
import genetic.Genetic._
import genetic.Functions._
import genetic.CompactGeneticAlgorithm.CGA
import genetic.PopulationBasedIncrementalLearning.PBIL

import scalax.chart._
import scalax.chart.Charting._
import scalax.chart.views._
import java.util.concurrent._


object Main {

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }

  def showResult(result: List[(Individual, Int)]) = {
    (1 to result.size).zip(result).foreach { case (i, (individual, evaluation)) =>
      println(i.toString ++ ": " ++ evaluation.toString ++ "|" ++ printIndividual(individual))
    }
  }

  def showChart(result: List[(Individual, Int)]) = {
    val values: Seq[(Int,Int)] = (1 to result.size).zip(result.map({ case ((individual, evaluation)) => evaluation })).toSeq
    val chart = XYLineChart(values.toXYSeriesCollection(), title = "Convergence of genetic algorithm")
    chart.show(scrollable = true)
  }

  def runPBILOneMaxMultiThreaded(noThreads: Int,
                                 populationSize: Int,
                                 individualSize: Int,
                                 steps: Int,
                                 th1: Double,
                                 th2: Double,
                                 th3: Double): List[(Individual, Int)] = {
    val threadIndividualSize = individualSize / noThreads
    val pool: ExecutorService = Executors.newFixedThreadPool(noThreads)
    def createFuture() = new FutureTask[List[(Individual, Int)]](new Callable[List[(Individual, Int)]]() {
      def call(): List[(Individual, Int)] = {
        PBIL(oneMax, populationSize, threadIndividualSize, steps, th1, th2, th3, new scala.util.Random)
      }})
    val futures  = List.fill(noThreads)(createFuture())
    futures.foreach { (f) =>  pool.execute(f) }
    val partials = time( futures.map( _.get() ) )
    val result: List[(Individual, Int)] = time ( partials.reduceLeft { (l1: List[(Individual, Int)], l2:List[(Individual, Int)]) =>
      (l1, l2).zipped map { case (((ind1,eva1), (ind2,eva2))) => (ind1++ind2, eva1+eva2) }
    } )
    pool.shutdown()
    result
  }

  def main(args: Array[String]) = {
    println("Running genetic algorithm...")
    //val result = CGA(oneMax, 1024, 15000, 0.004)
    //val result = time( PBIL(oneMax, 512, 1024, 1000, 0.001, 0.3, 0.1) )
    val result = time( runPBILOneMaxMultiThreaded(4, 128, 1024, 2000, 0.01, 0.003, 0.01) )

    println("Algorithm finished. Displaying chart...")
    showChart(result)

    //showResult(result)
  }

}
