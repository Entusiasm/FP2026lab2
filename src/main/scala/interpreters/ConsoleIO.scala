package interpreters

import algebra.Console
import cats.effect.IO

object ConsoleIO extends Console[IO] {
  def putStr(s: String): IO[Unit] = IO(print(s))
  def putStrLn(s: String): IO[Unit] = IO(println(s))
  def getStrLn: IO[String] = IO(scala.io.StdIn.readLine())
}