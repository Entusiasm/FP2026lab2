package domain

import domain.Helpers.combineMaps

object StateTransitions {

  def receiveMaterial(state: StateData, mats: Map[Material, Int]): StateData =
    state.copy(materials = combineMaps(state.materials, mats))

  def runAssembly(config: Config, state: StateData, product: Product, quantity: Int): (Vector[String], StateData, Either[String, (Int, Int)]) = {
    val recipe = config.recipes(product)
    val enough = recipe.materials.forall { case (m, need) => state.materials.getOrElse(m, 0) >= need * quantity }
    if (!enough) {
      (Vector(s"Не хватает материалов для $product"), state, Left("Недостаточно материалов"))
    } else {
      val newMats = recipe.materials.foldLeft(state.materials) { case (acc, (m, need)) =>
        acc.updated(m, acc(m) - need * quantity)
      }
      val defectCount = (quantity * config.actualDefectProbability).toInt
      val goodCount = quantity - defectCount
      val finished = combineMaps(state.finishedToys, Map(product -> goodCount))
      val defective = combineMaps(state.defectiveToys, Map(product -> defectCount))
      val newState = state.copy(materials = newMats, finishedToys = finished, defectiveToys = defective)
      val cost = (recipe.materials.map { case (m, qty) => config.materialCosts(m) * qty }.sum) * quantity
      val log = Vector(
        s"Производство: $quantity шт. $product",
        s"  - дефектных: $defectCount",
        s"  - стоимость: $cost руб.",
        s"  - брак: ${defectCount.toDouble / quantity * 100}%"
      )
      (log, newState, Right((goodCount, defectCount)))
    }
  }

  def inspectBatch(state: StateData, config: Config): (Vector[String], Boolean) = {
    val totalProduced = state.finishedToys.values.sum + state.defectiveToys.values.sum
    val totalDefective = state.defectiveToys.values.sum
    val defectRate = if (totalProduced == 0) 0.0 else totalDefective.toDouble / totalProduced * 100
    val allowed = config.allowedDefectRate >= defectRate
    val log = Vector(s"Инспекция: ${if (allowed) s"Уровень брака $defectRate% в норме." else s"Уровень брака $defectRate% ПРЕВЫШЕН!"}")
    (log, allowed)
  }

  def advanceShift(state: StateData, config: Config): (Vector[String], StateData) = {
    val newHours = state.shiftHours + 1
    val shiftEnded = newHours >= config.shiftDuration
    val finalHours = if (shiftEnded) 0 else newHours
    val log = Vector(s"Смена: ${state.shiftHours} -> $finalHours час(ов)") ++
      (if (shiftEnded) Vector("=== СМЕНА ЗАВЕРШЕНА ===") else Vector.empty)
    (log, state.copy(shiftHours = finalHours))
  }
}