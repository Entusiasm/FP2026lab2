package ui

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import domain.StateData
import algebra.Console

trait MenuOption[F[_]] {
  def title: String
  def execute(state: StateData)(implicit M: Monad[F], console: Console[F]): F[StateData]
}

case class MenuLeaf[F[_]](
  title: String,
  action: StateData => F[StateData]
) extends MenuOption[F] {
  def execute(state: StateData)(implicit M: Monad[F], console: Console[F]): F[StateData] = action(state)
}

case class MenuTreeNode[F[_]](
  title: String,
  children: Seq[MenuOption[F]]
) extends MenuOption[F] {
  def execute(state: StateData)(implicit M: Monad[F], console: Console[F]): F[StateData] = {
    def loop(s: StateData): F[StateData] = {
      for {
        _ <- console.putStrLn(s"\n=== $title ===")
        _ <- children.zipWithIndex.foldLeft(M.pure(())) { (acc, elem) =>
          val (opt, i) = elem
          acc.flatMap(_ => console.putStrLn(s"${i+1} - ${opt.title}"))
        }
        _ <- console.putStrLn("0 - назад")
        _ <- console.putStr("> ")
        input <- console.getStrLn
        cmd <- M.pure(input.toInt)
        newState <- cmd match {
          case n if 1 <= n && n <= children.size =>
            children(n-1).execute(s)
          case 0 => M.pure(s)
          case _ => console.putStrLn("Неверная команда").map(_ => s)
        }
        cont <- M.pure(cmd != 0)
        res <- if (cont) loop(newState) else M.pure(newState)
      } yield res
    }
    loop(state)
  }
}