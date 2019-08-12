package simianquant.jmhnuances

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.annotations._

object SingleSeq {

  @State(Scope.Thread)
  class InterpolatorInstance {
    val instance = Data.acmInterp
  }

  @State(Scope.Thread)
  class AbscissaeValue14 extends Data.BaseAbscissaeValue(14)

  @State(Scope.Thread)
  class SingleAbscissaValue {
    val x = math.Pi
  }

}

@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class SingleSeq {

  import SingleSeq._

  @Benchmark
  def baseline(interpolator: InterpolatorInstance, data: SingleAbscissaValue): Double = data.x

  @Benchmark
  def singleValue(interpolator: InterpolatorInstance, data: SingleAbscissaValue): Double =
    interpolator.instance.value(data.x)

  @Benchmark
  def sequenceValue(interpolator: InterpolatorInstance, data: AbscissaeValue14, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance.value(x))
      ctr += 1
    }
  }

}
