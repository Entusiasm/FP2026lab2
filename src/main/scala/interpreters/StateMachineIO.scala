package interpreters

import algebra.StateMachine
import cats.effect.IO
import cats.effect.Ref
import domain.{StateData, Product, Material, combineMaps, Config}

class StateMachineIO(
  stateRef: Ref[IO, StateData],
  config: Config,
  logging: LoggingIO
) extends StateMachine[IO]:
  def get: IO[StateData] = stateRef.get
  def set(s: StateData): IO[Unit] = stateRef.set(s)
  def modify(f: StateData => StateData): IO[Unit] = stateRef.update(f)

  def receiveMaterial(mats: Map[Material, Int]): IO[Unit] =
    modify(s => s.copy(materials = combineMaps(s.materials, mats))) >>
      logging.logReceiveMaterial(mats)

  def runAssembly(product: Product, quantity: Int): IO[Either[String, (Int, Int)]] =
    for
      s <- get
      recipe = config.recipes(product)
      enough = recipe.materials.forall { case (m, need) => s.materials.getOrElse(m, 0) >= need * quantity }
      result <- if enough then
        val newMats = recipe.materials.foldLeft(s.materials) { case (acc, (m, need)) =>
          acc.updated(m, acc(m) - need * quantity)
        }
        val defectCount = (quantity * config.actualDefectProbability).toInt
        val goodCount = quantity - defectCount
        val finished = combineMaps(s.finishedToys, Map(product -> goodCount))
        val defective = combineMaps(s.defectiveToys, Map(product -> defectCount))
        val newState = s.copy(materials = newMats, finishedToys = finished, defectiveToys = defective)
        val cost = recipe.materials.map { case (m, qty) => config.materialCosts(m) * qty }.sum * quantity
        for
          _ <- set(newState)
          _ <- logging.logProduction(product, quantity, defectCount, cost)
        yield Right((goodCount, defectCount))
      else
        logging.log(s"Не хватает материалов для $product").as(Left("Недостаточно материалов"))
    yield result

  def inspectBatch: IO[Boolean] =
    for
      s <- get
      totalProduced = s.finishedToys.values.sum + s.defectiveToys.values.sum
      totalDefective = s.defectiveToys.values.sum
      defectRate = if totalProduced == 0 then 0.0 else totalDefective.toDouble / totalProduced * 100
      allowed = config.allowedDefectRate >= defectRate
      _ <- logging.logInspection(defectRate, allowed)
    yield allowed

  def advanceShift: IO[Unit] =
    for
      s <- get
      newHours = s.shiftHours + 1
      ended = newHours >= config.shiftDuration
      finalHours = if ended then 0 else newHours
      _ <- set(s.copy(shiftHours = finalHours))
      _ <- logging.logShiftAdvance(s.shiftHours, finalHours, ended)
    yield ()