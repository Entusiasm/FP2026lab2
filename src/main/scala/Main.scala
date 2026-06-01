import cats.effect.IO
import cats.effect.unsafe.implicits.global
import domain.{ConfigData, InitialState}
import interpreters.{ConsoleIO, ConfigIO, LoggingIO, StateMachineIO}
import algebra.{Console, ConfigAlgebra, Logging, StateMachine}
import ui.MainMenu

object Main extends App {
  implicit val console: Console[IO] = ConsoleIO
  implicit val configAlg: ConfigAlgebra[IO] = new ConfigIO(ConfigData.initial)
  implicit val logging: Logging[IO] = new LoggingIO
  implicit val stateMachine: StateMachine[IO] = new StateMachineIO(ConfigData.initial)

  MainMenu.run[IO](InitialState.data).unsafeRunSync()
}