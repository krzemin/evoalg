package genetic



object Genetic {
  type Individual = List[Int]
  type VectorP = List[Double]
  type IndEvalFunction = (Individual => Int)
  type Point = (Int, Int)

  val rand = new scala.util.Random

  def uniformRandom(rnd: scala.util.Random = rand): Double = rnd.nextDouble()

  def binaryRandom(p : Double, rnd: scala.util.Random = rand) =
    if (uniformRandom(rnd) < p) 1 else 0

  def randomIndividual(p: VectorP, rnd: scala.util.Random = rand): Individual =
    p.map {(pk) => binaryRandom(pk, rnd) }

  def initVectorP(size: Int): VectorP =
    List.fill(size)(0.5)

  def orderIndividuals(F: IndEvalFunction)(x1: Individual, x2: Individual) =
    if(F(x1) > F(x2)) (x1, x2) else (x2, x1)

  def printIndividual(individual: Individual): String = individual match {
    case Nil => ""
    case x :: xs => x.toString() ++ printIndividual(xs)
  }


}
