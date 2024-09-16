package edu.neu.coe.csye7200.parse

import org.scalatest.{FlatSpec, Matchers}

class BenchmarkParserSpec extends FlatSpec with Matchers {

  behavior of "BenchmarkParser"

  val p = new BenchmarkParser()

  private val prefix1 = "2024-05-28 18:14:18.241 INFO TimeLogger"
  private val quadratic = "n^2"
  private val subQuadratic = "n^(4/3)"

  private val logStrings = Seq(
    "2024-05-29 16:44:08.089 INFO  TimeLogger - 1000@Bucket sort: Raw time per run {mSec}:  .2295",
    "2024-05-29 16:44:08.090 INFO  TimeLogger - 1000@Bucket sort: Normalized time per run {n}:  229.4998"
  )

  private val statsStrings = Seq(
    "2024-05-29 17:51:47.982 INFO  InstrumentedComparatorHelper - 1000@Bucket sort: StatPack {runs: 1 hits: mean=13818; stdDev=1893; normalized=0.020; copies: mean=730000; normalized=1.031; inversions: <unset>; swaps: mean=2955; stdDev=474; normalized=0.004; fixes: <unset>; compares: mean=3953; stdDev=473; normalized=0.006}",
    "2024-05-29 17:51:52.433 INFO  InstrumentedComparableHelper - 1000@LSD String Sort: StatPack {runs: 1 hits: mean=100000; normalized=0.646; copies: mean=40000; normalized=0.258; inversions: <unset>; swaps: mean=0; normalized=0.000; fixes: <unset>; compares: mean=0; normalized=0.000}"
  )

  it should "getLogEntries" in {
    val (q1, q2) = p.getLogEntries(logStrings.iterator)
    q1.size shouldBe 1
    q2.size shouldBe 1
  }

  it should "getStatsEntries" in {
    val result = p.getStatsEntries(statsStrings.iterator)
    result.size shouldBe 2
  }

  it should "logEntry" in {
    val entry = prefix1 + "- 1000@QuickSort dual pivot: Normalized time per run {n log n}:  17.5553"
    val x: p.ParseResult[LogEntry] = p.parseAll(p.logEntry, entry)
    x.successful shouldBe true
    x.get shouldBe LogEntry(1000, "QuickSort dual pivot", "Normalized", 17.5553)
  }
  it should "statsEntry" in {
    val prefix2 = "2024-05-29 15:08:13.808 INFO  InstrumentedComparatorHelper"
    val entry = prefix2 + "- 1000@Bucket sort : StatPack {runs: 1 hits: mean=13813"
    val x: p.ParseResult[StatsEntry] = p.parseAll(p.statsEntry, entry)
    x.successful shouldBe true
    x.get shouldBe StatsEntry(1000, "Bucket sort", 13813)
  }

  it should "kind1" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Raw time per run {mSec}")
    x.successful shouldBe true
    x.get shouldBe "Raw"
  }
  it should "kind2" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Normalized time per run {n log n}")
    x.successful shouldBe true
    x.get shouldBe "Normalized"
  }
  it should "kind3" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Normalized time per run {" + subQuadratic + "}")
    x.successful shouldBe true
    x.get shouldBe "Normalized"
  }
  it should "kind4" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Normalized time per run {" + quadratic + "}")
    x.successful shouldBe true
    x.get shouldBe "Normalized"
  }

  it should "description1" in {
    val quicksort = "QuickSort dual pivot"
    val x: p.ParseResult[String] = p.parseAll(p.description, quicksort)
    x.successful shouldBe true
    x.get shouldBe quicksort
  }

  it should "description2" in {
    val shellsort = "Shell sort in mode 4"
    val x: p.ParseResult[String] = p.parseAll(p.description, shellsort)
    x.successful shouldBe true
    x.get shouldBe shellsort
  }

  it should "description3" in {
    val shellsort = "string sort ASCII (Ext) with cutoff=256"
    val x: p.ParseResult[String] = p.parseAll(p.description, shellsort)
    x.successful shouldBe true
    x.get shouldBe "string sort ASCII Ext with cutoff"
  }

  it should "prefix1" in {
    val x: p.ParseResult[String] = p.parseAll(p.prefix, prefix1)
    x.successful shouldBe true
    x.get shouldBe prefix1
  }

  it should "date" in {
    val x: p.ParseResult[String] = p.parseAll(p.date, "2024-05-28")
    x.successful shouldBe true
    x.get shouldBe "2024-05-28"
  }

  it should "time" in {
    val x: p.ParseResult[String] = p.parseAll(p.time, "18:14:18.241")
    x.successful shouldBe true
    x.get shouldBe "18:14:18.241"
  }
  it should "formula" in {
    val x: p.ParseResult[String] = p.parseAll(p.formula, quadratic)
    x.successful shouldBe true
    x.get shouldBe quadratic
  }
  it should "anything" in {
    val x: p.ParseResult[String] = p.parseAll(p.formula, subQuadratic)
    x.successful shouldBe true
    x.get shouldBe subQuadratic
  }

  it should "parseBenchmarkLogEntry" in {
    val entry = prefix1 + "- 1000@QuickSort dual pivot: Normalized time per run {n log n}:  17.5553"
    p.parseBenchmarkLogEntry(entry) should matchPattern { case scala.util.Success(_) => }
  }

}