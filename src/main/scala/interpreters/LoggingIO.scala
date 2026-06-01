package interpreters

import algebra.Logging
import cats.effect.IO
import domain.{Product, Material}

class LoggingIO extends Logging[IO] {
  def log(msg: String): IO[Unit] = IO(println(s"[LOG] $msg"))
  def logProduction(product: Product, quantity: Int, defectCount: Int, cost: Int): IO[Unit] = {
    log(s"Производство: $quantity шт. $product") >>
    log(s"  - дефектных: $defectCount") >>
    log(s"  - стоимость: $cost руб.") >>
    log(s"  - брак: ${defectCount.toDouble / quantity * 100}%")
  }
  def logReceiveMaterial(materials: Map[Material, Int]): IO[Unit] =
    log(s"Получены материалы: ${materials.mkString(", ")}")
  def logInspection(defectRate: Double, allowed: Boolean): IO[Unit] = {
    val msg = if (allowed) s"Уровень брака $defectRate% в норме." else s"Уровень брака $defectRate% ПРЕВЫШЕН!"
    log(s"Инспекция: $msg")
  }
  def logShiftAdvance(prevHours: Int, newHours: Int, shiftEnded: Boolean): IO[Unit] = {
    log(s"Смена: $prevHours -> $newHours час(ов)") >>
      (if (shiftEnded) log("=== СМЕНА ЗАВЕРШЕНА ===") else IO.unit)
  }
}