package simianquant.jmhnuances

import annotation.tailrec
import spire.math.{Jet, JetDim}
import spire.implicits._

final class SpireLinearInterpolator(private val xs: Array[Jet[Double]],
                                    private val yc: Array[Array[Jet[Double]]],
                                    private val y0: Jet[Double],
                                    private val yEnd: Jet[Double])(implicit private val jetDim: JetDim)
    extends Serializable {

  private val xlb = xs(0).real
  private val xlen = xs.length
  private val xub = xs(xlen - 1).real

  final def apply(x: Jet[Double]): Jet[Double] =
    if (x.real < xlb)
      Jet(y0.real, y0.infinitesimal.clone())
    else if (x.real >= xub)
      Jet(yEnd.real, yEnd.infinitesimal.clone())
    else {
      val idx = getIdx(x.real)
      val coeffs = yc(idx)
      coeffs(0) + (x - xs(idx)) * coeffs(1)
    }

  private def getIdx(x: Double): Int = {
    @tailrec
    def go(low: Int, high: Int): Int = {
      val mid = (low + high) / 2
      if (low == mid)
        low
      else if (x < xs(mid).real)
        go(low, mid)
      else
        go(mid, high)
    }
    go(0, xs.length - 1)
  }

}

object SpireLinearInterpolator {

  def apply(xsi: Seq[Jet[Double]], ysi: Seq[Jet[Double]])(implicit jetDim: JetDim): SpireLinearInterpolator = {
    val xs = xsi.toArray.clone()
    val ys = ysi.toArray.clone() // cloned to check against mutation while calibrating
    val xlen = xs.length

    val yc = Array.ofDim[Jet[Double]](xlen - 1, 2)
    var ctr = 0
    while (ctr < xlen - 1) {
      val intercept = ys(ctr)
      val slope = (ys(ctr + 1) - ys(ctr)) / (xs(ctr + 1) - xs(ctr))
      yc(ctr) = Array(intercept, slope)
      ctr += 1
    }
    new SpireLinearInterpolator(xs, yc, ys(0), ys(xlen - 1))
  }

}
