package genetic

import Genetic._

object Functions {

  def classifier(tg_alpha: Double)(p: Point): Boolean = p match {
    case (x, y) => tg_alpha * x - y >= 0
  }

  def tangensifyIndividual(ind: Individual): Double = {
    val longValue: Long = ind.foldLeft(0)((n, bit) => (n << 1) | (bit & 1))
    math.abs(longValue).toDouble * math.Pi / Long.MaxValue
  }

  def objectiveFun(P: Vector[Point], orig_classif: (Point => Boolean))(ind: Individual): Int = {
    val tg_alpha: Double = tangensifyIndividual(ind)
    val our_classif: (Point => Boolean) = classifier(tg_alpha)
    val training = P.take(650)
    (for {
      point <- training
      if orig_classif(point) == our_classif(point)
    } yield 1).sum
  }


}
