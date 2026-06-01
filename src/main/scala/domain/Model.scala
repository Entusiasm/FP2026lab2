package domain

sealed trait Product
case object TeddyBear extends Product
case object Doll extends Product
case object Car extends Product

sealed trait Material
case object Fabric extends Material
case object Wood extends Material
case object Plastic extends Material
case object Paint extends Material

case class Recipe(materials: Map[Material, Int])

case class Config(
  recipes: Map[Product, Recipe],
  materialCosts: Map[Material, Int],
  shiftDuration: Int,
  allowedDefectRate: Double,
  actualDefectProbability: Double
)

case class StateData(
  materials: Map[Material, Int],
  finishedToys: Map[Product, Int],
  defectiveToys: Map[Product, Int],
  shiftHours: Int
)

object Helpers {
  def combineMaps[K](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] =
    (a.keySet ++ b.keySet).map(k => k -> (a.getOrElse(k, 0) + b.getOrElse(k, 0))).toMap
}

object ConfigData {
  val initial: Config = Config(
    recipes = Map(
      TeddyBear -> Recipe(Map(Fabric -> 2, Paint -> 1)),
      Doll -> Recipe(Map(Fabric -> 3, Plastic -> 1, Paint -> 2)),
      Car -> Recipe(Map(Plastic -> 4, Wood -> 1, Paint -> 1))
    ),
    materialCosts = Map(Fabric -> 10, Wood -> 15, Plastic -> 5, Paint -> 3),
    shiftDuration = 8,
    allowedDefectRate = 10.0,
    actualDefectProbability = 0.05
  )
}

object InitialState {
  val data: StateData = StateData(
    materials = Map(Fabric -> 50, Wood -> 20, Plastic -> 100, Paint -> 30),
    finishedToys = Map.empty,
    defectiveToys = Map.empty,
    shiftHours = 0
  )
}