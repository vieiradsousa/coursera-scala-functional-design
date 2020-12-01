package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal ( b() * b() - 4.0 * a() * c() )

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta_ = computeDelta(a, b, c)
      val squared_delta = Signal(Math.sqrt(delta_()))
      val neg_b = Signal(b() * -1.0)
      val divisor = Signal(2.0 * a())

      if (delta_() == 0) Set(neg_b() / divisor())
      else if (delta_() > 0) Set((neg_b() + squared_delta()) / divisor(), (neg_b() - squared_delta()) / divisor())
      else Set.empty
    }
  }
}
