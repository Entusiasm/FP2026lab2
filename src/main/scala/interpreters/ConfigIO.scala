package interpreters

import algebra.ConfigAlgebra
import cats.effect.IO
import domain.{Config, Product, Recipe, Material}

class ConfigIO(config: Config) extends ConfigAlgebra[IO] {
  def recipeOf(product: Product): IO[Recipe] = IO.pure(config.recipes(product))
  def productionCost(product: Product, quantity: Int): IO[Int] = IO.pure {
    val recipe = config.recipes(product)
    val costPerUnit = recipe.materials.map { case (m, qty) => config.materialCosts(m) * qty }.sum
    costPerUnit * quantity
  }
  def canProduce(product: Product, quantity: Int, available: Map[Material, Int]): IO[Boolean] = IO.pure {
    val recipe = config.recipes(product)
    recipe.materials.forall { case (m, need) => available.getOrElse(m, 0) >= need * quantity }
  }
  def isDefectAllowed(currentDefectRate: Double): IO[Boolean] = IO.pure(config.allowedDefectRate >= currentDefectRate)
}