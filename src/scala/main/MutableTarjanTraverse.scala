package main

import scala.annotation.tailrec
import scala.collection.mutable

case class MutableTarjanTraverse(
                                  currentId: Int,
                                  idsAssignments: Array[Int],
                                  lowLinks: Array[Int],
                                  stack: mutable.Stack[Int],
                                  stackIndex: Array[Boolean],
                                  sccs: List[List[Int]]
                                ) extends TarjanTraverseAlgebra {
  private val UNVISITED = Int.MinValue

  override def getSccs = sccs

  override def isCurrentIdEqualsToLowLink(id: Int): Boolean = idsAssignments(id) == lowLinks(id)

  override def assign(at: Int): TarjanTraverseAlgebra = {
    idsAssignments(at) = id
    stack.push(at)
    stackIndex(at) = true
    lowLinks(at) = id
    copy(currentId = currentId + 1)
  }

  override def id: Int = currentId

  override def isVisited(id: Int): Boolean = idsAssignments(id) != UNVISITED

  override def updateLowValueIfInStack(from: Int, to: Int): TarjanTraverseAlgebra = {
    if (stackIndex(to)) lowLinks(from) = math.min(lowLinks(from), lowLinks(to))
    copy()
  }

  override def getLowLinks = lowLinks

  def collectScc(sccId: Int): TarjanTraverseAlgebra = {
    @tailrec
    def iterate(acc: List[Int]): List[Int] = {
      if (stack.nonEmpty) {
        val node = stack.pop()
        stackIndex(node) = false
        lowLinks(node) = idsAssignments(sccId)
        if (node != sccId) iterate(node :: acc)
        else node :: acc
      } else {
        acc
      }
    }

    val scc = iterate(List())

    if (scc.nonEmpty) copy(sccs = scc :: sccs)
    else this
  }
}

object MutableTarjanTraverse {
  def empty(n: Int) = MutableTarjanTraverse(0, Array.fill(n)(Int.MinValue), Array.fill(n)(Int.MinValue), mutable.Stack[Int](), Array.fill(n)(false), List())
}