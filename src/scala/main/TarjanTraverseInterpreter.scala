package main

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Stack}

case class TarjanTraverseInterpreter(
                                      currentId: Int,
                                      assignedId: Vector[Int],
                                      visited: BitSet,
                                      stack: Stack[Int],
                                      lowLinks: Vector[Int],
                                      stackIndex: BitSet,
                                      sccs: List[List[Int]]
                                    ) extends TarjanTraverseAlgebra {

  override def isCurrentIdEqualsToLowLink(id: Int): Boolean = assignedId(id) == lowLinks(id)

  override def assign(at: Int): TarjanTraverseAlgebra = copy(
    currentId = currentId + 1,
    assignedId = assignedId.updated(at, id),
    stack = stack.push(at),
    lowLinks = lowLinks.updated(at, id),
    visited = visited + at,
    stackIndex = stackIndex + at
  )

  override def id: Int = currentId

  override def isVisited(id: Int): Boolean = visited(id)

  def updateLowValueIfInStack(from: Int, to: Int): TarjanTraverseAlgebra =
    if (stackIndex(to)) copy(lowLinks = lowLinks.updated(from, math.min(lowLinks(from), lowLinks(to))))
    else this

  def getSccs: List[List[Int]] = sccs

  override def getLowLinks = lowLinks

  def collectScc(sccId: Int): TarjanTraverseAlgebra = {
    case class SccCollectIterationContext(sccAcc: List[Int], stackIndex: BitSet, lowLinks: Vector[Int], stack: Stack[Int])

    @tailrec
    def iterate(ctx: SccCollectIterationContext): SccCollectIterationContext = if (stack.nonEmpty) {
      ctx.stack.pop2 match {
        case (node, stackUpd) =>
          val ctxUpd = SccCollectIterationContext(
            sccAcc = node :: ctx.sccAcc,
            stackIndex = ctx.stackIndex - node,
            lowLinks = ctx.lowLinks.updated(node, assignedId(sccId)),
            stack = stackUpd
          )
          if (node != sccId) iterate(ctxUpd)
          else ctxUpd
      }
    } else {
      ctx
    }


    val processed = iterate(SccCollectIterationContext(List(), stackIndex, lowLinks, stack))
    val sccsUpdated = if (processed.sccAcc.isEmpty) sccs else processed.sccAcc :: sccs
    copy(sccs = sccsUpdated, lowLinks = processed.lowLinks, stackIndex = processed.stackIndex, stack = processed.stack)
  }
}

object TarjanTraverseInterpreter {
  def empty(n: Int) = TarjanTraverseInterpreter(
    currentId = 0,
    assignedId = Vector.fill(n)(Int.MinValue),
    visited = BitSet(),
    stack = Stack[Int](),
    lowLinks = Vector.fill(n)(Int.MinValue),
    stackIndex = BitSet(),
    sccs = List()
  )
}