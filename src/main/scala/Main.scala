import Main.dbConnection
import zio.*
import com.github.tototoshi.csv.*
import zio.stream.ZSink
import Treatments.*
import SubMenu.*
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
      _ <- printLine("1. If you want a list of all departments and regions")
      _ <- printLine("2. Number of gas stations in chosen department or region")
      _ <- printLine("3. Average price of gas in chosen department or region")
      _ <- printLine("4. Most present extra services in gas stations")
      _ <- printLine("5. Department with the most gas stations")
      _ <- printLine("6. Most expensive gas type")
      _ <- printLine("7. Average number of extra services per station")
      _ <- printLine("8. Average price of gas for specific extra services")
      _ <- printLine("Please enter your choice (or 'q' to quit):")
      choice <- readLine.orDie
      _ <- processChoice(choice)
    } yield ()

  def processChoice(choice: String): ZIO[Any, Any, Unit] =
    choice match {
      case "1" =>
        printRegionsAndDepartments() *> printMenu
      case "2" =>
        regionOrDepartment(choice, dbConnection) *> printMenu
      case "3" =>
        regionOrDepartment(choice, dbConnection) *> printMenu
      case "4" =>
        calculateMostPresentExtraService(dbConnection) *> printMenu
      case "5" =>
        findDepartmentWithMostGasStations(dbConnection) *> printMenu
      case "6" =>
        calculateMostExpensiveGas(dbConnection) *> printMenu
      case "7" =>
        calculateAverageExtraServicesPerStation() *> printMenu
      case "8" =>
        calculateAveragePriceForExtraServicesWithZStream() *> printMenu
      case "q" =>
        printLine("Exiting...") *> ZIO.unit
      case _ =>
        printLine("Invalid choice. Please enter a valid option.") *> printMenu
    }
}
