package rpm4s

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import scodec.Codec
import scodec.bits.BitVector
import rpm4s.codecs._
import rpm4s.data.Dependency._
import rpm4s.data._
import shapeless.{::, HNil}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class DecodingBench {

  @Benchmark
  def noop(): Unit = {
    // this method was intentionally left blank.
  }

  @Benchmark
  def decodeLead(): Unit = {
    Codec[Lead].decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeRpm(): Unit = {
    Codec[RpmFile].decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeHeaderRange(): Unit = {
    DecodingBench.headerRange.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeProvides(): Unit = {
    DecodingBench.provides.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeNameProvidesHlist(): Unit = {
    DecodingBench.nameProvidesHlist.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeFileEntries(): Unit = {
    DecodingBench.fileEntries.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeArch(): Unit = {
    DecodingBench.arch.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeNameArchHlist(): Unit = {
    DecodingBench.nameArchHlist.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeRpmPrimaryEntry(): Unit = {
    DecodingBench.rpmPrimaryEntryQuery.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeAllPkgRefs(): Unit = {
    DecodingBench.allPkgRefs.decode(DecodingBench.kernelPackageCompact)
  }

  @Benchmark
  def decodeChangeLogEntries(): Unit = {
    DecodingBench.changeLogEntries.decode(DecodingBench.kernelPackageCompact)
  }

}

object DecodingBench {
  val headerRange = decoder[HeaderRange]
  val provides = decoder[List[Provides]]
  val nameProvidesHlist = decoder[Name :: List[Provides] :: HNil]
  val allPkgRefs = decoder[List[Requires] :: List[Provides] :: List[Obsoletes] :: List[
    Enhances] :: List[Conflicts] :: List[Supplements] :: List[Recommends] :: List[
    Suggests] :: List[FileEntry] :: HNil]
  val fileEntries = decoder[List[FileEntry]]
  val arch = decoder[Architecture]
  val nameArchHlist = decoder[Name :: Architecture :: HNil]
  val rpmPrimaryEntryQuery = decoder[RpmPrimaryEntry]
  val changeLogEntries = decoder[List[ChangeLogEntry]]

  val kernelPackageCompact = BitVector
    .fromInputStream(getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))
    .compact

}
