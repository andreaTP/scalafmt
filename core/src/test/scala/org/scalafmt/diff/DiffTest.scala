package org.scalafmt.diff

import org.scalatest.FunSuite

class DiffTest extends FunSuite {
  test("parse") {
    val diff =
      """|--- /dev/null
         |+++ b/core/src/test/scala/org/scalafmt/DiffTest.scala
         |@@ -54 +54,2 @@ class Router(formatOps: FormatOps) {
         |-  import Constants._
         |+  import
         |+  Constants._
         |@@ -57 +58 @@ class Router(formatOps: FormatOps) {
         |-  import TreeOps._
         |+  import  TreeOps._
         |@@ -60,3 +61,6 @@ class Router(formatOps: FormatOps) {
         |-  private def getSplits(formatToken: FormatToken): Seq[Split] = {
         |-    val style = styleMap.at(formatToken)
         |-    val leftOwner = owners(formatToken.left)
         |+  private def getSplits(""".stripMargin

    val expected = List(
      FileDiff("core/src/test/scala/org/scalafmt/DiffTest.scala",
               List(Addition(54, 2), Addition(58, 1), Addition(61, 6))))
    val obtained = FileDiff.fromUnified(diff)
    assert(obtained == expected)
  }

}
