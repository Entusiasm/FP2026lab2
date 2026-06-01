package algebra

trait Console[F[_]] {
  def putStr(s: String): F[Unit]
  def putStrLn(s: String): F[Unit]
  def getStrLn: F[String]
}