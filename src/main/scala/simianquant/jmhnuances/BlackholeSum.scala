package simianquant.jmhnuances

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.annotations._

object BlackholeSum {

  @State(Scope.Thread)
  class InterpolatorInstance {
    val instance = Data.acmInterp
  }

  @State(Scope.Thread)
  class AbscissaeValue14 extends Data.BaseAbscissaeValue(14)

}

@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class BlackholeSum {

  import BlackholeSum._

  @Benchmark
  def noBlackhole(interpolator: InterpolatorInstance, data: AbscissaeValue14): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      interpolator.instance.value(x)
      ctr += 1
    }
  }

  @Benchmark
  def valueSum(interpolator: InterpolatorInstance, data: AbscissaeValue14): Double = {
    var ctr = 0
    var res = 0.0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      res += interpolator.instance.value(x)
      ctr += 1
    }
    res
  }

  @Benchmark
  def valueBlackhole(interpolator: InterpolatorInstance, data: AbscissaeValue14, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance.value(x))
      ctr += 1
    }
  }

}
