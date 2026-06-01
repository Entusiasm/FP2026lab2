package ui

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import algebra.{Console, ConfigAlgebra, Logging, StateMachine}
import domain._

object MenuActions {

  private def readInt[F[_]: Monad](console: Console[F], prompt: String): F[Int] =
    console.putStr(prompt).flatMap { _ =>
      console.getStrLn.flatMap { input =>
        val n = if (input.matches("\\d+")) input.toInt else -1
        Monad[F].pure(if (n > 0) n else -1)
      }
    }

  private def readProduct[F[_]: Monad](console: Console[F]): F[Option[Product]] =
    console.putStrLn("Товар: 1 - Мишка, 2 - Кукла, 3 - Машинка").flatMap { _ =>
        console.getStrLn.map { input =>
            scala.util.Try(input.toInt).toOption match {
                case Some(1) => Some(TeddyBear)
                case Some(2) => Some(Doll)
                case Some(3) => Some(Car)
                case _ => None
            }
        }
    }

    private def readMaterial[F[_]: Monad](console: Console[F]): F[Option[Material]] =
    console.putStrLn("Материал: 1 - Ткань, 2 - Дерево, 3 - Пластик, 4 - Краска").flatMap { _ =>
        console.getStrLn.map { input =>
            scala.util.Try(input.toInt).toOption match {
                case Some(1) => Some(Fabric)
                case Some(2) => Some(Wood)
                case Some(3) => Some(Plastic)
                case Some(4) => Some(Paint)
                case _ => None
            }
        }
    }

  def receiveMaterial[F[_]: Monad: Console: StateMachine: Logging](
    state: StateData
  ): F[StateData] = {
    readMaterial(implicitly[Console[F]]).flatMap { maybeMat =>
      maybeMat match {
        case Some(mat) =>
          readInt(implicitly[Console[F]], "Количество: ").flatMap { qty =>
            if (qty > 0) {
              val mats = Map(mat -> qty)
              implicitly[StateMachine[F]].receiveMaterial(state, mats).flatMap { case (newState, _) =>
                implicitly[Console[F]].putStrLn("Материалы добавлены.").map(_ => newState)
              }
            } else {
              implicitly[Console[F]].putStrLn("Количество должно быть положительным").map(_ => state)
            }
          }
        case None =>
          implicitly[Console[F]].putStrLn("Неверный материал").map(_ => state)
      }
    }
  }

  def assemble[F[_]: Monad: Console: StateMachine: Logging](
    state: StateData
  ): F[StateData] = {
    readProduct(implicitly[Console[F]]).flatMap { maybeProduct =>
      maybeProduct match {
        case Some(product) =>
          readInt(implicitly[Console[F]], "Количество: ").flatMap { qty =>
            if (qty > 0) {
              implicitly[StateMachine[F]].runAssembly(state, product, qty).flatMap {
                case (newState, Right((good, defect))) =>
                  implicitly[Console[F]].putStrLn(s"Произведено: $good годных, $defect бракованных.").map(_ => newState)
                case (newState, Left(err)) =>
                  implicitly[Console[F]].putStrLn(s"Ошибка: $err").map(_ => newState)
              }
            } else {
              implicitly[Console[F]].putStrLn("Количество должно быть положительным").map(_ => state)
            }
          }
        case None =>
          implicitly[Console[F]].putStrLn("Неверный товар").map(_ => state)
      }
    }
  }

  def inspect[F[_]: Monad: Console: StateMachine](
    state: StateData
  ): F[StateData] = {
    implicitly[StateMachine[F]].inspectBatch(state).flatMap { case (newState, allowed) =>
      if (!allowed) implicitly[Console[F]].putStrLn("ВНИМАНИЕ: брак превышен!").map(_ => newState)
      else Monad[F].pure(newState)
    }
  }

  def advance[F[_]: Monad: Console: StateMachine](
    state: StateData
  ): F[StateData] = {
    implicitly[StateMachine[F]].advanceShift(state).flatMap { case (newState, _) =>
      if (newState.shiftHours == 0) implicitly[Console[F]].putStrLn("Смена окончена.").map(_ => newState)
      else Monad[F].pure(newState)
    }
  }

  def showState[F[_]: Monad: Console](
    state: StateData
  ): F[StateData] = {
    val console = implicitly[Console[F]]
    console.putStrLn("\n=== ТЕКУЩЕЕ СОСТОЯНИЕ ===").flatMap { _ =>
      console.putStrLn(s"Материалы: ${state.materials}").flatMap { _ =>
        console.putStrLn(s"Готовые игрушки: ${state.finishedToys}").flatMap { _ =>
          console.putStrLn(s"Брак: ${state.defectiveToys}").flatMap { _ =>
            console.putStrLn(s"Часы смены: ${state.shiftHours}").flatMap { _ =>
              console.putStrLn("===========================\n").map(_ => state)
            }
          }
        }
      }
    }
  }

  def mainMenu[F[_]: Monad: Console: StateMachine: Logging]: MenuTreeNode[F] = {
    MenuTreeNode("ГЛАВНОЕ МЕНЮ", Seq(
      MenuLeaf("Получить материалы", receiveMaterial[F]),
      MenuLeaf("Собрать партию игрушек", assemble[F]),
      MenuLeaf("Инспекция брака", inspect[F]),
      MenuLeaf("Перейти к следующему часу смены", advance[F]),
      MenuLeaf("Показать состояние", showState[F])
    ))
  }
}