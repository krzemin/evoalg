package cga

object CGA {
  val rand = scala.util.Random
  val d = 48
  
  type ProbabilityVector = List[Double]
  type Individual = List[Int]
  
  def binaryRandom(p: Double): Int =
    if (rand.nextDouble < p) 1 else 0
  
  def initProbabilityVector: ProbabilityVector =
    List.fill(d)(0.5)
  
  def randomIndividual(p: ProbabilityVector): Individual =
    p.map(binaryRandom)
  
  def orderIndividuals(F: Function[Individual, Int])(x1: Individual, x2: Individual) =
    if(F(x1) > F(x2)) (x1, x2) else (x2, x1)
  
  var n = 1000
  def terminationCondition: Boolean = {
    n = n - 1
    n == 0
  }
  
  def printVector(x: Individual): String = x match {
    case Nil => ""
    case (x :: xs) => x.toString ++ printVector(xs)
  }
  
  def CGA(F: Function[Individual, Int])(theta: Double) = {
    var p = initProbabilityVector
    var x1 = randomIndividual(p)
    var x2 = randomIndividual(p)
    while (!terminationCondition) {
      print("x1: ", printVector(x1), " rank: ", F(x1))
      println(" x2: ", printVector(x2), " rank: ", F(x2))
      
      val (best, worst) = orderIndividuals(F)(x1, x2)
      val pbw = p.zip(best.zip(worst))
      p = pbw.map( it => it match {
        case (pi, (1, 0)) => pi + theta
        case (pi, (0, 1)) => pi - theta
        case (pi, _) => pi
      })
      x1 = randomIndividual(p)
      x2 = randomIndividual(p)
    }
    (x1, x2)
  }
  
  
  def oneMax(x: Individual) = x.sum
  def pattern(p: Individual)(x: Individual) =
    p.zip(x).count((p) => p match { case (a, b) => a == b } )
  
}


object Main {

  def main(args: Array[String]): Unit = {
    println(CGA.CGA(CGA.oneMax)(0.01))
  }

}
