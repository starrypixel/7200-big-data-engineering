package edu.neu.coe.csye7200.parse

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

object BenchmarkParser extends App {

  val p: BenchmarkParser = new BenchmarkParser()
  val source = Source.fromFile("/Users/rhillyard/IdeaProjects/INFO6205/savedLogs/bm20240730.txt")
  val ws: Iterator[String] = for {line <- source.getLines() if line.contains("@")} yield line
  val (ss, ls): (Iterator[String], Iterator[String]) = ws.partition(w => w.contains("StatPack"))
  val (q1, q2) = p.getLogEntries(ls)
  val xs = p.getStatsEntries(ss)

  import java.io._

  val pw = new PrintWriter(new File("output.csv"))
  pw.write("N\tKind\tAlgorithm\tvalue\n")
  pw.write((q1 map (_.render)) mkString "\n")
  pw.write("\n")
  pw.write((q2 map (_.render)) mkString "\n")
  pw.write("\n")
  pw.write((xs map (_.render)) mkString "\n")
  pw.write("\n")
  pw.close()
}

class BenchmarkParser extends JavaTokenParsers {
  def getLogEntries(ls: Iterator[String]): (Seq[LogEntry], Seq[LogEntry]) = {
    val lys: Iterator[Try[LogEntry]] = ls map (w => parseBenchmarkLogEntry(w))
    val lsy: Try[Seq[LogEntry]] = lys.foldLeft(Try(Seq[LogEntry]())) {
      (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
    }
    lsy match {
      case scala.util.Success(xs) => xs partition (e => e.kind == "Raw")
      case scala.util.Failure(x) => throw x
    }
  }

  def getStatsEntries(ss: Iterator[String]): Seq[StatsEntry] = {
    val z = ss.toSeq map (w => w.replaceAll(";.*", "").replaceAll(",", ""))
    val xys: scala.Seq[Try[StatsEntry]] = z map (w => parseBenchmarkStatsEntry(w))
    val xsy: Try[Seq[StatsEntry]] = xys.foldLeft(Try(Seq[StatsEntry]())) {
      (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
    }

    xsy match {
      case scala.util.Success(xs) => xs
      case scala.util.Failure(x) => throw x

    }
  }

  def parseBenchmarkLogEntry(s: String): Try[LogEntry] = parseAll(logEntry, s) match {
    case this.Success(x, _) => scala.util.Success(x)
    case this.Failure(m, _) => scala.util.Failure(ParserException(s"parseBenchmarkLogEntry: unable to parse '$s' because: $m"))
    case this.Error(m, _) => scala.util.Failure(new Exception(m))
  }

  def parseBenchmarkStatsEntry(s: String): Try[StatsEntry] = parseAll(statsEntry, s) match {
    case this.Success(x, _) => scala.util.Success(x)
    case this.Failure(m, _) => scala.util.Failure(ParserException(s"parseBenchmarkStatsEntry: unable to parse '$s' because: $m"))
    case this.Error(m, _) => scala.util.Failure(new Exception(m))
  }

  def logEntry: Parser[LogEntry] = prefix ~ ("-" ~> wholeNumber) ~ ("@" ~> description) ~ (":" ~> kind) ~ (":" ~> floatingPointNumber) ^^ {
    case _ ~ x ~ w ~ k ~ t => LogEntry(x.toInt, w, k, t.toDouble)
  }

  def statsEntry: Parser[StatsEntry] = prefix ~ ("-" ~> wholeNumber) ~ ("@" ~> description) ~ (": StatPack {runs:" ~> wholeNumber) ~
          ("hits: mean=" ~> floatingPointNumber) ^^ {
    case _ ~ x ~ w ~ _ ~ h => StatsEntry(x.toInt, w, h.toDouble)
  }

  def description: Parser[String] = rep(term) ~ opt(wholeNumber) ~ opt(cutoff) ^^ { case xs ~ no ~ _ => xs.mkString(" ") + formatOption(no) }

  def term: Parser[String] = "(" ~> ident <~ ")" | ident <~ "=" <~ wholeNumber | ident | failure("badly formed term")

  def cutoff: Parser[String] = "with cutoff=" ~> wholeNumber

  private def formatOption(no: Option[String]) = no match {
    case Some(n) => s" $n"
    case None => ""
  }

  def kind: Parser[String] = ident <~ "time per run " <~ "{" <~ formula <~ "}"

  def formula: Parser[String] = "mSec" | "n log n" | "n^2" | "n^(4/3)" | "n" | failure("bad formula")

  def prefix: Parser[String] = date ~ time ~ ident ~ ident ^^ { case d ~ t ~ x ~ z => s"$d $t $x $z" }

  def date: Parser[String] = """\d{4}-\d{2}-\d{2}""".r

  def time: Parser[String] = """\d{2}:\d{2}:\d{2}\.\d{3}""".r
}

case class LogEntry(n: Int, name: String, kind: String, time: Double) {
  def render: String = s"$n\t$kind\t$name\t$time"
}


case class StatsEntry(n: Int, name: String, hits: Double) {
  def render: String = s"$n\tHits\t$name\t$hits"
}