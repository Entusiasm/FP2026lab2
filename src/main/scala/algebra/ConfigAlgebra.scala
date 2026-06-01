package algebra

import domain.{Product, Recipe, Material}

trait ConfigAlgebra[F[_]] {
  def recipeOf(product: Product): F[Recipe]
  def productionCost(product: Product, quantity: Int): F[Int]
  def canProduce(product: Product, quantity: Int, available: Map[Material, Int]): F[Boolean]
  def isDefectAllowed(currentDefectRate: Double): F[Boolean]
}