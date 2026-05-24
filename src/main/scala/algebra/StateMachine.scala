package algebra

import domain.{StateData, Product, Material}

trait StateMachine[F[_]]:
  def get: F[StateData]
  def set(s: StateData): F[Unit]
  def modify(f: StateData => StateData): F[Unit]
  def receiveMaterial(mats: Map[Material, Int]): F[Unit]
  def runAssembly(product: Product, quantity: Int): F[Either[String, (Int, Int)]]
  def inspectBatch: F[Boolean]
  def advanceShift: F[Unit]