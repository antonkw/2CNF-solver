package main


trait TarjanTraverseAlgebra {

  /**
   * Current current id
   *
   * @return
   */
  def id: Int

  /**
   * Increment id, push it to stack and update low link
   *
   * @param at source id
   * @return updated state
   */
  def assign(at: Int): TarjanTraverseAlgebra

  /**
   * Is node visited.
   *
   * @param id ud to check
   */
  def isVisited(id: Int): Boolean

  /**
   * Check if current id equal to it's lowlink value.
   *
   * @param id id to check
   */
  def isCurrentIdEqualsToLowLink(id: Int): Boolean


  /**
   * Update lowlink for 'from' to min lowlink (choise between lowlink for 'from' and 'to' nodes); previously presence in stack is checked.
   *
   * @param from
   * @param to
   * @return updated state
   */
  def updateLowValueIfInStack(from: Int, to: Int): TarjanTraverseAlgebra

  /**
   * Accessor for strongly connected components
   *
   * @return collected components
   */
  def getSccs: List[List[Int]]

  /**
   * Accessor for lowlinks (lowlink can be interpreted as id of strongly collected component.
   *
   * @return
   */
  def getLowLinks: IndexedSeq[Int]

  /**
   * Collect strongly connected component. Iteration will stop when first node of scc will be found.
   *
   * @param sccId id of strongly connected component
   * @return updated state with new component
   */
  def collectScc(sccId: Int): TarjanTraverseAlgebra
}