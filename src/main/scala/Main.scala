import cats.effect.{IO, IOApp}
import cats.effect.kernel.Ref
import program.FactoryProgram
import domain.{ConfigData, InitialState}
import interpreters.{ConsoleIO, ConfigIO, LoggingIO, StateMachineIO}
import cats.Monad

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    for
      stateRef <- Ref.of[IO, domain.StateData](InitialState.data)
      configIO = new ConfigIO(ConfigData.initial)
      loggingIO = new LoggingIO
      stateMachineIO = new StateMachineIO(stateRef, ConfigData.initial, loggingIO)
      _ <- FactoryProgram.run[IO](
        ConsoleIO,
        configIO,
        loggingIO,
        stateMachineIO
      )(using Monad[IO])
    yield ()