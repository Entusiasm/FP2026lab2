package ui

import cats.Monad
import cats.syntax.flatMap._
import algebra.{Console, StateMachine, Logging}
import domain.StateData

object MainMenu {
  def run[F[_]: Monad: Console: StateMachine: Logging](initialState: StateData): F[Unit] = {
    val menu = MenuActions.mainMenu[F]
    menu.execute(initialState).flatMap(_ => implicitly[Console[F]].putStrLn("Программа завершена."))
  }
}