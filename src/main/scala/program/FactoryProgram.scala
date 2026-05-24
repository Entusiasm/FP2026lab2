package program

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import algebra.{Console, ConfigAlgebra, Logging, StateMachine}
import domain.{Product, Material, TeddyBear, Doll, Car, Fabric, Wood, Plastic, Paint}

object FactoryProgram:

  private def readInt[F[_]: Monad](
    console: Console[F],
    prompt: String
  ): F[Int] =
    for
      _ <- console.putStr(prompt)
      input <- console.getStrLn
      value <- input.toIntOption match
        case Some(i) if i > 0 => Monad[F].pure(i)
        case _ => Monad[F].pure(-1)
    yield value

  private def readProduct[F[_]: Monad](
    console: Console[F]
  ): F[Option[Product]] =
    for
      _ <- console.putStrLn("Товар: 1 - Мишка, 2 - Кукла, 3 - Машинка")
      input <- console.getStrLn
    yield input.toIntOption match
      case Some(1) => Some(TeddyBear)
      case Some(2) => Some(Doll)
      case Some(3) => Some(Car)
      case _ => None

  private def readMaterial[F[_]: Monad](
    console: Console[F]
  ): F[Option[Material]] =
    for
      _ <- console.putStrLn("Материал: 1 - Ткань, 2 - Дерево, 3 - Пластик, 4 - Краска")
      input <- console.getStrLn
    yield input.toIntOption match
      case Some(1) => Some(Fabric)
      case Some(2) => Some(Wood)
      case Some(3) => Some(Plastic)
      case Some(4) => Some(Paint)
      case _ => None

  private def showState[F[_]: Monad](
    console: Console[F],
    stateMachine: StateMachine[F]
  ): F[Unit] =
    for
      s <- stateMachine.get
      _ <- console.putStrLn("\n=== СОСТОЯНИЕ ФАБРИКИ ===")
      _ <- console.putStrLn(s"Материалы: ${s.materials}")
      _ <- console.putStrLn(s"Готовые: ${s.finishedToys}")
      _ <- console.putStrLn(s"Брак: ${s.defectiveToys}")
      _ <- console.putStrLn("==========================\n")
    yield ()

  private def handleCommand[F[_]: Monad](
    cmd: Int,
    console: Console[F],
    configAlg: ConfigAlgebra[F],
    logging: Logging[F],
    stateMachine: StateMachine[F]
  ): F[Boolean] = cmd match
    case 1 =>
      for
        maybeMat <- readMaterial(console)
        _ <- maybeMat match
          case Some(mat) =>
            for
              qty <- readInt(console, "Количество: ")
              _ <- if qty > 0 then
                for
                  _ <- logging.logReceiveMaterial(Map(mat -> qty))
                  _ <- stateMachine.receiveMaterial(Map(mat -> qty))
                  _ <- console.putStrLn("Материалы добавлены.")
                yield ()
              else console.putStrLn("Количество должно быть положительным")
            yield ()
          case None =>
            console.putStrLn("Неверный материал")
      yield true

    case 2 =>
      for
        maybeProduct <- readProduct(console)
        _ <- maybeProduct match
          case Some(product) =>
            for
              qty <- readInt(console, "Количество: ")
              _ <- if qty > 0 then
                for
                  result <- stateMachine.runAssembly(product, qty)
                  _ <- result match
                    case Right((good, defect)) =>
                      console.putStrLn(s"Произведено: $good годных, $defect бракованных.")
                    case Left(err) =>
                      console.putStrLn(s"Ошибка: $err")
                yield ()
              else console.putStrLn("Количество должно быть положительным")
            yield ()
          case None =>
            console.putStrLn("Неверный товар")
      yield true

    case 3 =>
      for
        allowed <- stateMachine.inspectBatch
        _ <- if !allowed then console.putStrLn("ВНИМАНИЕ: брак превышен!") else Monad[F].pure(())
      yield true

    case 4 =>
      for
        _ <- stateMachine.advanceShift
        s <- stateMachine.get
        _ <- if s.shiftHours == 0 then console.putStrLn("Смена окончена.") else Monad[F].pure(())
      yield true

    case 5 =>
      showState(console, stateMachine).as(true)

    case 0 =>
      for
        _ <- console.putStrLn("До свидания!")
      yield false

    case _ =>
      for
        _ <- console.putStrLn("Неверная команда")
      yield true

  def run[F[_]: Monad](
    console: Console[F],
    configAlg: ConfigAlgebra[F],
    logging: Logging[F],
    stateMachine: StateMachine[F]
  ): F[Unit] =
    def loop: F[Unit] =
      for
        _ <- console.putStrLn("\n=== МЕНЮ ===")
        _ <- console.putStrLn("1 - Получить материалы")
        _ <- console.putStrLn("2 - Собрать партию")
        _ <- console.putStrLn("3 - Инспекция брака")
        _ <- console.putStrLn("4 - Следующий час")
        _ <- console.putStrLn("5 - Показать состояние")
        _ <- console.putStrLn("0 - Выход")
        _ <- console.putStr("> ")
        input <- console.getStrLn
        cmd <- Monad[F].pure(input.toIntOption.getOrElse(-1))
        cont <- handleCommand(cmd, console, configAlg, logging, stateMachine)
        _ <- if cont then loop else Monad[F].pure(())
      yield ()
    loop