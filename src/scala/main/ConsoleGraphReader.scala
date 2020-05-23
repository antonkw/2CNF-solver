package main

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.util.StringTokenizer

import scala.collection.mutable.ArrayBuffer


case class Clause(a: Int, b: Int)

case class CNF(variablesCount: Int, clauses: Seq[Clause])

case class Edge(s: Int, t: Int)


class ConsoleReader {
  val in: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
  var tok = new StringTokenizer("")

  @throws[IOException]
  def nextInt: Int = next.toInt

  @throws[IOException]
  def next: String = {
    while ( {
      !tok.hasMoreElements
    }) tok = new StringTokenizer(in.readLine)
    tok.nextToken
  }
}

object ConsoleGraphReader {
  def readCnf: CNF = {
    val fastScanner = new ConsoleReader()

    val vertexCount = fastScanner.nextInt
    val clauseCount = fastScanner.nextInt
    val input = ArrayBuffer[Clause]()

    for (_ <- 0 until clauseCount) {
      val e1 = fastScanner.nextInt
      val e2 = fastScanner.nextInt
      input += Clause(e1, e2)
    }

    CNF(vertexCount, input)
  }
}