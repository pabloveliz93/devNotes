import scala.annotation.tailrec
import scala.math._

object ExponentialIntegral {

  private val EulerMascheroni = 0.5772156649015329
  private val Tolerance = 1e-15
  private val MaxIterations = 100

  def expi(x: Double): Double = x match {
    case 0.0 => Double.NegativeInfinity
    case _ if x < 0 =>
      val absX = -x
      if (absX > 40.0) {
        // For large negative values, use asymptotic expansion of E1
        // Ei(x) = -E1(-x) for x < 0
        -asymptoticE1(absX)
      } else {
        // For moderate negative values, use series expansion
        seriesExpansionNegative(x)
      }
    case _ if x < 51.0 => seriesExpansion(x)
    case _ => asymptotic(x)
  }

  private def asymptoticE1(x: Double): Double = {
    // Asymptotic expansion for E1(x) when x is large
    // E1(x) ~ e^(-x)/x * [1 - 1/x + 2!/x^2 - 3!/x^3 + ...]
    val maxTerms = 30
    val threshold = 1e-15
    var term = 1.0
    var sum = term
    var n = 1

    while (n < maxTerms) {
      term *= -n / x  // Note the negative sign for E1 asymptotic series
      sum += term
      if (abs(term) < abs(sum) * threshold) {
        return (exp(-x) / x) * sum
      }
      n += 1
    }

    (exp(-x) / x) * sum
  }

  private def seriesExpansionNegative(x: Double): Double = {
    // For negative x, use the same series but with proper handling
    @tailrec
    def loop(term: Double, n: Int, sum: Double): Double = {
      if (abs(term) <= Tolerance || n >= MaxIterations) sum
      else {
        val nextTerm = term * x / (n + 1)
        loop(nextTerm, n + 1, sum + term / n)
      }
    }

    EulerMascheroni + log(abs(x)) + loop(x, 1, 0.0)
  }

  private def seriesExpansion(x: Double): Double = {
    @tailrec
    def loop(term: Double, n: Int, sum: Double): Double = {
      if (abs(term) <= Tolerance || n >= MaxIterations) sum
      else {
        val nextTerm = term * x / (n + 1)
        loop(nextTerm, n + 1, sum + term / n)
      }
    }

    EulerMascheroni + log(abs(x)) + loop(x, 1, 0.0)
  }

  private def asymptotic(x: Double): Double = {
    val maxTerms = 30
    val threshold = 1e-15
    var term = 1.0
    var sum = term
    var n = 1

    while (n < maxTerms) {
      term *= n / x
      sum += term
      if (abs(term) < abs(sum) * threshold) {
        return (exp(x) / x) * sum
      }
      n += 1
    }

    (exp(x) / x) * sum
  }

  private def expint1(x: Double): Double = {
    require(x > 0, "expint1 requires positive argument")

    if (x < 1.0) seriesE1(x)
    else continuedFractionE1(x)
  }

  private def seriesE1(x: Double): Double = {
    @tailrec
    def loop(term: Double, n: Int, sum: Double, sign: Int): Double = {
      if (abs(term) <= Tolerance || n >= MaxIterations) sum
      else {
        val nextTerm = term * x / (n + 1)
        loop(nextTerm, n + 1, sum + sign * term / n, -sign)
      }
    }

    -EulerMascheroni - log(x) + loop(-x, 1, 0.0, -1)
  }

  private def continuedFractionE1(x: Double): Double = {
    @tailrec
    def loop(i: Int, prev: Double, curr: Double): Double = {
      if (i > 20) curr
      else {
        val next = 1.0 + i.toDouble / (x + i.toDouble / curr)
        if (abs(next - curr) < 1e-12) next
        else loop(i + 1, curr, next)
      }
    }

    exp(-x) / (x + loop(1, 0.0, 1.0))
  }

  def calculateExpected(s1: Double, s2: Double, a: Double, scaled: Double): Double = {
    val term1 = expi(s1 * scaled)
    val term2 = expi(s2 * scaled)
    1.0 - (-term1 + term2) / a
  }

}
