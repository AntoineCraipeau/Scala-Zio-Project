import zio.*
import zio.Console.*
import com.github.tototoshi.csv.*
import zio.stream.ZSink
import Treatments.*
import zio.stream.ZStream

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter = ';'
}

object Main extends ZIOAppDefault {
  override def run: ZIO[Any & (ZIOAppArgs & Scope), Any, Unit] =
    for {
      _ <- printLine("Welcome to Gas Station Treatments!")
      _ <- printLine("1. Number of gas stations in chosen department or region")
      _ <- printLine("2. Average price of gas in chosen department or region")
      _ <- printLine("3. Most present extra services in gas stations")
      _ <- printLine("4. Department with the most gas stations")
      _ <- printLine("5. Most expensive gas type")
      _ <- printLine("6. Exit")
      _ <- printLine("Please enter your choice:")
      choice <- readLine.orDie
      stream <- choice match {
        case "1" =>
          regionOrDepartment(choice)
        case "2" =>
          regionOrDepartment(choice)
        case "3" =>
          calculateMostPresentExtraService()
        case "4" =>
          findDepartmentWithMostGasStations()
        case "5" =>
          calculateMostExpensiveGas()
        case "6" =>
          ZIO.succeed(ZStream.empty)
        case _ =>
          printLine("Invalid choice. Please enter a valid option.")
      }
    } yield ()
}