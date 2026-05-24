package algebra

import domain.{Product, Material}

trait Logging[F[_]]:
  def log(msg: String): F[Unit]
  def logProduction(product: Product, quantity: Int, defectCount: Int, cost: Int): F[Unit]
  def logReceiveMaterial(materials: Map[Material, Int]): F[Unit]
  def logInspection(defectRate: Double, allowed: Boolean): F[Unit]
  def logShiftAdvance(prevHours: Int, newHours: Int, shiftEnded: Boolean): F[Unit]