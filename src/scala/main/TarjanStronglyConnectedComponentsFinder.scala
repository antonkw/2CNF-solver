package main

class TarjanStronglyConnectedComponentsFinder(traverseStateInterpeterInitialState: TarjanTraverseAlgebra) {

  def findSccs(adjList: Map[Int, List[Int]]): TarjanTraverseAlgebra = {
    def dfs(sourceNode: Int, state: TarjanTraverseAlgebra): TarjanTraverseAlgebra = {
      val sourceNodeAssignment = state.assign(sourceNode)
      val subgraphAssignment = adjList.getOrElse(sourceNode, List()).foldLeft(sourceNodeAssignment)(
        (state, targetNode) =>
          (if (!state.isVisited(targetNode)) dfs(targetNode, state) else state).updateLowValueIfInStack(sourceNode, targetNode)
      )

      if (subgraphAssignment.isCurrentIdEqualsToLowLink(sourceNode)) {
        subgraphAssignment.collectScc(sourceNode)
      } else {
        subgraphAssignment
      }
    }

    traverseStateInterpeterInitialState.getLowLinks.indices
      .foldLeft(traverseStateInterpeterInitialState)((s, i) => if (s.isVisited(i)) s else dfs(i, s))
  }
}

object TarjanStronglyConnectedComponentsFinder {
  def apply(nodeCount: Int): TarjanStronglyConnectedComponentsFinder =
    new TarjanStronglyConnectedComponentsFinder(TarjanTraverseInterpreter.empty(nodeCount))
}
