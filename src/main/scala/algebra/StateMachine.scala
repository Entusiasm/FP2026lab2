package algebra

import domain.{StateData, Material, Product}

trait StateMachine[F[_]] {
  def receiveMaterial(state: StateData, mats: Map[Material, Int]): F[(StateData, Unit)]
  def runAssembly(state: StateData, product: Product, quantity: Int): F[(StateData, Either[String, (Int, Int)])]
  def inspectBatch(state: StateData): F[(StateData, Boolean)]
  def advanceShift(state: StateData): F[(StateData, Unit)]
}