import GasType._
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.DefaultCSVFormat
import zio.Console.*
import zio.ZIO.*
import GasStation.*
import zio.stream.*
import zio._
import zio.Config.Bool

object SubMenu{

    implicit object CustomFormat extends DefaultCSVFormat 
    {
        override val delimiter = ';'
    }

    def regionOrDepartment(value: String): ZIO[Any, Any, Unit] = {
        for {
        _ <- printLine("Do you want to search by region or department ? (r/d)")
        choice <- readLine.orDie
        region <- choice match {
            case "r" => ZIO.succeed(true)
            case "d" => ZIO.succeed(false)
            case _ => 
            printLine("Invalid choice. Please enter a valid option.") *> regionOrDepartment(value)
        }
        _ <- if (region == true) Treatments.regionCount(value) else Treatments.departmentCount(value)
        } yield ()
    }

    def printRegionsAndDepartments(): ZIO[Any, Any, Unit] = {
        for {
        _ <- printAllRegions()
        _ <- printAllDepartments()
        } yield ()
    }

    def printAllRegions(): ZIO[Any, Any, Unit] = {
        for {
        _ <- printLine("List of all regions :")
        _ <- ZIO.foreach(Region.values) { region =>
            printLine(s"${region.code}: ${region.name}")
        }
        _ <- printLine(" \n ")
        } yield ()
    }

    def printAllDepartments(): ZIO[Any, Any, Unit] = {
        for {
        _ <- printLine("List of all departments :")
        _ <- ZIO.foreach(Department.values) { department =>
            printLine(s"${department.code}: ${department.name}")
        }
        _ <- printLine(" \n ")
        } yield ()
    }
}
