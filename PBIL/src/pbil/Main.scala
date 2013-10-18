package pbil

object PBIL {
  
  val rand = scala.util.Random
  val d = 128
  
  type ProbabilityVector = List[Double]
  type Individual = List[Int]
  type Population = List[Individual]
  
  def binaryRandom(p: Double): Int =
    if (rand.nextDouble < p) 1 else 0
  
  def initProbabilityVector: ProbabilityVector =
    List.fill(d)(0.5)
  
  def randomIndividual(p: ProbabilityVector): Individual =
    p.map(binaryRandom)
  
  def randomPopulation(p: ProbabilityVector, N: Int): Population =
    List.fill(N)(randomIndividual(p))
  
  def orderIndividuals(F: Function[Individual, Int])(x1: Individual, x2: Individual) =
    if(F(x1) > F(x2)) (x1, x2) else (x2, x1)
  
  var n = 500
  def terminationCondition: Boolean = {
    n = n - 1
    n == 0
  }
  
  def printVector(x: Individual): String = x match {
    case Nil => ""
    case (x :: xs) => x.toString ++ printVector(xs)
  }
  
  def bestIndividual(F: Function[Individual, Int])(pop: Population): Individual = pop.maxBy(F)
  
  def PBIL(F: Function[Individual, Int])(N: Int, th1: Double, th2: Double, th3: Double): Individual = {
    val p = initProbabilityVector
    val population = randomPopulation(p, N)
    
    while(!terminationCondition) {
      val best = bestIndividual(F)(population)
      
    }
    
  }
  
}


object Main {

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }

}
