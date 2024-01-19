import Main.dbConnection
import zio.*
import com.github.tototoshi.csv.*
import zio.stream.ZSink
import Treatments.*
import zio.stream.ZStream
import zio.Console.{printLine, *}

import java.sql.{Connection, DriverManager, SQLException}

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter = ';'
}
object Main extends ZIOAppDefault {

  val dbUrl = "jdbc:h2:mem:testdb;DB_CLOSE_DELAY=-1"
  val user = "sa"
  val password = ""

  val dbConnection = DriverManager.getConnection(dbUrl, user, password)
  


  override def run: ZIO[Any & (ZIOAppArgs & Scope), Any, Unit] =
    for {

      _ <- createTableIfNotExists_GasStationsByRegDept(dbConnection)
      _ <- createTableIfNotExists_AvgGasPricesByRegDept(dbConnection)
      _ <- createTableIfNotExists_MostPresentGasStationServices(dbConnection)
      _ <- createTableIfNotExists_DptMostGasStations(dbConnection)
      _ <- createTableIfNotExists_MostExpensiveGasType(dbConnection)

      _ <- printLine("Welcome to Gas Station Streams !")
      _ <- printMenu
    } yield ()

  def printMenu: ZIO[Any, Any, Unit] =
    for {
      _ <- printLine("1. Number of gas stations in chosen department or region")
      _ <- printLine("2. Average price of gas in chosen department or region")
      _ <- printLine("3. Most present extra services in gas stations")
      _ <- printLine("4. Department with the most gas stations")
      _ <- printLine("5. Most expensive gas type")
      _ <- printLine("Please enter your choice (or 'q' to quit):")
      choice <- readLine.orDie
      _ <- processChoice(choice)
    } yield ()

  def processChoice(choice: String): ZIO[Any, Any, Unit] =
    choice match {
      case "1" =>
        regionOrDepartment(choice, dbConnection) *> printMenu
      case "2" =>
        regionOrDepartment(choice, dbConnection) *> printMenu
      case "3" =>
        calculateMostPresentExtraService(dbConnection) *> printMenu
      case "4" =>
        findDepartmentWithMostGasStations(dbConnection) *> printMenu
      case "5" =>
        calculateMostExpensiveGas(dbConnection) *> printMenu
      case "q" =>
        printLine("Exiting...") *> ZIO.unit
      case _ =>
        printLine("Invalid choice. Please enter a valid option.") *> printMenu
    }
}
