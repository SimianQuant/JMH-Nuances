package simianquant.jmhnuances

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.annotations._

object MultiSeq {

  @State(Scope.Thread)
  class InterpolatorInstance {
    val instance = Data.spireInterp
  }

  @State(Scope.Thread)
  class AbscissaeJet10 extends Data.BaseAbscissaeJet(10)

  @State(Scope.Thread)
  class AbscissaeJet12 extends Data.BaseAbscissaeJet(12)

  @State(Scope.Thread)
  class AbscissaeJet14 extends Data.BaseAbscissaeJet(14)

  @State(Scope.Thread)
  class AbscissaeJet16 extends Data.BaseAbscissaeJet(16)

  @State(Scope.Thread)
  class AbscissaeJet18 extends Data.BaseAbscissaeJet(18)

  @State(Scope.Thread)
  class AbscissaeJet20 extends Data.BaseAbscissaeJet(20)

}
@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class MultiSeq {

  import MultiSeq._

  @Benchmark
  def jet10(interpolator: InterpolatorInstance, data: AbscissaeJet10, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance(x))
      ctr += 1
    }
  }

  @Benchmark
  def jet12(interpolator: InterpolatorInstance, data: AbscissaeJet12, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance(x))
      ctr += 1
    }
  }

  @Benchmark
  def jet14(interpolator: InterpolatorInstance, data: AbscissaeJet14, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance(x))
      ctr += 1
    }
  }

  @Benchmark
  def jet16(interpolator: InterpolatorInstance, data: AbscissaeJet16, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance(x))
      ctr += 1
    }
  }

  @Benchmark
  def jet18(interpolator: InterpolatorInstance, data: AbscissaeJet18, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance(x))
      ctr += 1
    }
  }

  @Benchmark
  def jet20(interpolator: InterpolatorInstance, data: AbscissaeJet20, bh: Blackhole): Unit = {
    var ctr = 0
    while (ctr < data.xs.length) {
      val x = data.xs(ctr)
      bh.consume(interpolator.instance(x))
      ctr += 1
    }
  }

}
