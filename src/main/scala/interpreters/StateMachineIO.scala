package interpreters

import cats.effect.IO
import algebra.StateMachine
import domain.{Config, StateData, Material, Product}
import domain.StateTransitions

class StateMachineIO(config: Config) extends StateMachine[IO] {
  def receiveMaterial(state: StateData, mats: Map[Material, Int]): IO[(StateData, Unit)] =
    IO.pure((StateTransitions.receiveMaterial(state, mats), ()))

  def runAssembly(state: StateData, product: Product, quantity: Int): IO[(StateData, Either[String, (Int, Int)])] = {
    val (log, newState, result) = StateTransitions.runAssembly(config, state, product, quantity)
    // Лог можно игнорировать или вывести через logging, но для простоты оставим так
    IO.pure((newState, result))
  }

  def inspectBatch(state: StateData): IO[(StateData, Boolean)] = {
    val (log, allowed) = StateTransitions.inspectBatch(state, config)
    IO.pure((state, allowed))
  }

  def advanceShift(state: StateData): IO[(StateData, Unit)] = {
    val (log, newState) = StateTransitions.advanceShift(state, config)
    IO.pure((newState, ()))
  }
}