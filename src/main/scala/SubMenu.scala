import zio.Console.*
import zio.ZIO.*
import zio.*
import java.sql.Connection

object SubMenu{

    def regionOrDepartment(value: String, dbConnection: Connection): ZIO[Any, Any, Unit] = {
        for {
        _ <- printLine("Do you want to search by region or department ? (r/d)")
        choice <- readLine.orDie
        region <- choice match {
            case "r" => ZIO.succeed(true)
            case "d" => ZIO.succeed(false)
            case _ => 
            printLine("Invalid choice. Please enter a valid option.") *> regionOrDepartment(value, dbConnection)
        }
        _ <- if (region == true) Treatments.regionCount(value, dbConnection) else Treatments.departmentCount(value, dbConnection)
        } yield ()
    }

    def printRegionsAndDepartments(): ZIO[Any, Any, Unit] = {
        for {
        _ <- printAllRegions()
        _ <- printAllDepartments()
        } yield ()
    }

    private def printAllRegions(): ZIO[Any, Any, Unit] = {
        for {
        _ <- printLine("List of all regions :")
        _ <- ZIO.foreachDiscard(Region.values) {
            region =>
                printLine(s"${region.code}: ${region.name}")
        }
        _ <- printLine(" \n ")
        } yield ()
    }

    private def printAllDepartments(): ZIO[Any, Any, Unit] = {
        for {
        _ <- printLine("List of all departments :")
        _ <- ZIO.foreachDiscard(Department.values) {
            department =>
                printLine(s"${department.code}: ${department.name}")
        }
        _ <- printLine(" \n ")
        } yield ()
    }
}
