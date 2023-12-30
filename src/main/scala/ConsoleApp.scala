import zio.*
import zio.Console.*

object ConsoleApp {

  def runMenu(): ZIO[Console, Any, Unit] = {
    for {
      _ <- printLine("Choose a treatment:")
      _ <- printLine("1. Gas stations open 24/24 in Île-de-France")
      _ <- printLine("2. Average price of gas in Île-de-France")
      choice <- readLine.orDie
      _ <- choice match {
        case "1" =>
          Treatments.countStations()
        case "2" =>
          Treatments.averagePrice()
        case _ =>
          printLine("Invalid choice. Please enter a valid option.")
      }
    } yield ()
  }
}
