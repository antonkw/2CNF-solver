import main._


sealed trait CnfSolution

case class Solution(variableValues: List[Int]) extends CnfSolution

case class NoSolution() extends CnfSolution


class CnfSolver {

  def solve(cnf: CNF) = {
    val implicationGraph: ImplicationGraph = buildImplicationGraph(cnf)
    val tarjanSolution = TarjanStronglyConnectedComponentsFinder(implicationGraph.nodeCount).findSccs(implicationGraph.adjacencyList)
    val (sccs, index) = (tarjanSolution.getSccs.reverse, tarjanSolution.getLowLinks)

    def negation(node: Int) = if (node < cnf.variablesCount) node + cnf.variablesCount else node - cnf.variablesCount

    val isSatisfiable = sccs.forall(_.forall(node => index(node) != index(negation(node))))

    if (isSatisfiable) {
      val emptyAssignments = Vector.fill(cnf.variablesCount)(0)

      def toPos(postion: Int) = if (postion < cnf.variablesCount) postion else postion - cnf.variablesCount

      val boolValAssigned = sccs.foldLeft(emptyAssignments)((assignments, scc) => scc.foldLeft(assignments)((assignments2, node) => {
        val pos = toPos(node)
        if (assignments2(pos) == 0) assignments2.updated(pos, if (node < cnf.variablesCount) 1 else -1)
        else assignments2
      }))

      Solution(boolValAssigned.zipWithIndex.map { case (coef, idx) => coef * (idx + 1) }.toList)
    } else {
      NoSolution
    }

  }

  /**
   * Build implication graph from CNF
   * Main used low is: (a ∨ b) == (¬a -> b) ∧ (¬b -> a)
   *
   * @param cnf CNF input
   * @return implication graph (list of edges)
   */
  private def buildImplicationGraph(cnf: CNF): ImplicationGraph = {

    /**
     * Maps source element to positive and negative positions
     *
     * @param el element to conver
     * @return 2 positions
     */
    def positions(el: Int): (Int, Int) = if (el > 0) (el - 1, el + cnf.variablesCount - 1) else (-el + cnf.variablesCount - 1, -el - 1)

    val edges: Seq[Edge] = cnf.clauses.flatMap(clause => {
      val (a, notA) = positions(clause.a)
      val (b, notB) = positions(clause.b)
      List(Edge(notA, b), Edge(notB, a))
    })

    val adjacencyList: Map[Int, List[Int]] = edges.map(e => e.s -> e.t).groupBy(_._1).mapValues(_.toList.map(_._2))

    ImplicationGraph(
      nodeCount = cnf.variablesCount * 2,
      adjacencyList = adjacencyList
    )
  }

  case class ImplicationGraph(nodeCount: Int, adjacencyList: Map[Int, List[Int]])

}

object CnfSolver extends App {
  val cnf: CNF = ConsoleGraphReader.readCnf
  val solver = new CnfSolver

  val t = new Thread(null, new Runnable {
    def run() {
      solver.solve(cnf) match {
        case Solution(vars) =>
          println("SATISFIABLE")
          println(vars.mkString(" "))
        case NoSolution =>
          println("UNSATISFIABLE")
      }
    }
  }, "cnfThread", 100000000L)

  t.start()
  t.join()
}
