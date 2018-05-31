package StandbyMe.parser

import scala.annotation.tailrec

class ClosureUtil(productionSet: ProductionSet) {
  val first: First = compute_First(productionSet)

  def apply(item: Item): Closure = {
    val init_workbench = Set(item)
    val init_result: ItemSet = Set()
    helper(init_workbench)(init_result)
  }

  def first_helper(β: Vector[SS])(a: SS): Set[SS] = {
    if (β.isEmpty) Set(a)
    else first(β.head)
  }

  def single_step(item: Item): Closure = {
    resolve(item) match {
      case Some(matchResult) =>
        val B = matchResult.B
        val β = matchResult.β
        val a = matchResult.a
        val r__set: Set[Vector[SS]] = productionSet.withFilter(_._1 == B).map(_._2)
        for {
          b <- first_helper(β)(a)
          r <- r__set
        } yield (B, Vector(), r, b)
      case None => Set()
    }
  }

  def single_step(itemSet: ItemSet): Closure = itemSet.flatMap(single_step)

  @tailrec
  final def helper(workbench: ItemSet)(result: ItemSet): ItemSet = {
    if (workbench.isEmpty) result
    else {
      val next_result = result ++ workbench
      val next_workbench = single_step(workbench).diff(next_result)
      helper(next_workbench)(next_result)
    }
  }
}
