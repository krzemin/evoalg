
import genetic.Genetic._
import genetic.Functions._
import genetic.CompactGeneticAlgorithm.CGA


object Main {

  def randomPoint(): Point = (rand.nextInt(), rand.nextInt())


  def main(args: Array[String]) = {
    println("Running genetic algorithm...")

    val k = rand.nextDouble()
    val P: Vector[Point] = Vector.fill(1000)(randomPoint())
    val classif: (Point => Boolean) = classifier(k)
    val objFun: (Individual => Int) = objectiveFun(P, classif)

    println(k)
    val result = CGA(objFun, 64, 200, 0.015)
    val guess_k = tangensifyIndividual(result)
    println(guess_k)
  }

}
