package org.scalafmt.diff

import scala.util.Try

case class Addition(startLine: Int, lineCount: Int)
case class FileDiff(filename: String, additions: Seq[Addition])

object FileDiff {
  val NewFile = "^\\+\\+\\+\\ (.*?/)(\\S*)".r
  val DiffBlock =
    "^@@.*\\+(\\d+)(,(\\d+))?".r("startLine", "skip", "lineCount")

  /** Parses a unified diff into FileDiffs.
    *
    * Example commands to produce unified diff:
    *    git diff -U0 HEAD
    *    svn diff --diff-cmd=diff -x-U0
    *
    * Produces something like:
    *    --- /dev/null
    *    +++ b/DiffTest.scala
    *    @@ -54 +54,2 @@ class Router(formatOps: FormatOps) {
    *
    * Example parsed result:
    *    Seq(FileDiff("DiffTest.scala", Seq(Addition(54, 2))))
    */
  def fromUnified(diff: String): Seq[FileDiff] = {
    var currentFilename = Option.empty[String]
    val fileDiffs = Seq.newBuilder[FileDiff]
    val additions = Seq.newBuilder[Addition]
    def addLastFile(): Unit = {
      currentFilename.foreach { lastFilename =>
        fileDiffs += FileDiff(lastFilename, additions.result())
        additions.clear()
      }
    }
    diff.lines.foreach {
      case NewFile(_, filename) =>
        addLastFile()
        currentFilename = Some(filename)
      case other =>
        for {
          diffBlock <- DiffBlock.findAllMatchIn(other)
          startLine <- Try(diffBlock.group("startLine").toInt).toOption.toIterable
          lineCount = Try(diffBlock.group("lineCount").toInt).getOrElse(1)
        } {
          additions += Addition(startLine, lineCount)
        }
    }
    addLastFile()
    fileDiffs.result()
  }
}
