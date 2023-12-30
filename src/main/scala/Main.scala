import zio.*
import zio.Console.*
import com.github.tototoshi.csv.*
import zio.stream.ZSink


implicit object CustomFormat extends DefaultCSVFormat { // Notre fichier CSV utilise le délimiteur ';' au lieu de ','
  override val delimiter = ';'
}

object App extends ZIOAppDefault {
  override def run: ZIO[Any & (ZIOAppArgs & Scope), Any, Unit] =
    for {
      _ <- printLine("Welcome to Gas Station Treatments!")
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