package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      if (delta() < 0)
        Set()
      else if (delta() == 0)
        Set(-1 * b() / (2 * a()))
      else {
        val sqrtDelta = Math.sqrt(delta())
        Set((-1 * b() + sqrtDelta) / (2 * a()), (-1 * b() - sqrtDelta) / (2 * a()))
      }
    )
  }
}
