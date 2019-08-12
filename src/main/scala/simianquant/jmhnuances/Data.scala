package simianquant.jmhnuances

import org.apache.commons.math3.analysis.interpolation.LinearInterpolator
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import spire.implicits._
import spire.math.{Jet, JetDim}

/** Holds the data vectors to construct the interpolation objects and data ranges
  *
  * @author Harshad Deo
  * @since 0.1.0
  */
object Data {

  private val xsd =
    Array(-2.0, -1.8, -1.6, -1.4, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8)
  private val ysd = Array(-0.19, -0.12, -0.02, 0.12, 0.28, 0.45, 0.62, 0.78, 0.9, 0.97, 1.0, 0.97, 0.9, 0.78, 0.62,
    0.45, 0.28, 0.12, -0.02, -0.12)

  implicit val _jetdim = new JetDim(20)

  private val xsj = xsd map (x => Jet(x))
  private val ysj = ysd.zipWithIndex map (x => Jet(x._1) + Jet.h[Double](x._2))

  private val xdlb = xsd(0)
  private val xdub = xsd(xsd.length - 1)

  private val xjlb = xsj(0)
  private val xjub = xsj(xsj.length - 1)

  /** Constructs the data value vector over which the interpolator is evaluated
    *
    * @author Harshad Deo
    * @since 0.1.0
    */
  abstract class BaseAbscissaeValue(cntShift: Int) {
    private val cnt = 1 << cntShift
    private val incr = (xdub - xdlb) / cnt
    val xs: Array[Double] = new Array[Double](cnt)
    private var i = 0
    while (i < cnt) {
      xs(i) = xdlb + incr * i
      i += 1
    }
  }

  /** Constructs the data jet vector over which the interpolator is evaluated
    *
    * @author Harshad Deo
    * @since 0.1.0
    */
  abstract class BaseAbscissaeJet(cntShift: Int) {
    private val cnt = 1 << cntShift
    private val incr = (xjub - xjlb) / cnt
    val xs: Array[Jet[Double]] = Array.ofDim[Jet[Double]](cnt)
    private var i = 0
    while (i < cnt) {
      xs(i) = xjlb + incr * i
      i += 1
    }
  }

  /** Linear interpolator using Apache Commons Math
    *
    * @author Harshad Deo
    * @since 0.1.0
    */
  def acmInterp: PolynomialSplineFunction = new LinearInterpolator().interpolate(xsd, ysd)

  /** Linear interpolator using Spire
    *
    * @author Harshad Deo
    * @since 0.1.0
    */
  def spireInterp: SpireLinearInterpolator = SpireLinearInterpolator(xsj, ysj)

}
