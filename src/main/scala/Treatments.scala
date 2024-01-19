import GasType._
import Streams.*
import zio.Console.*
import zio.ZIO.*
import zio._

import java.sql.Connection
import scala.util.Try

object Treatments{

  //Calculate the number of gas stations in a department
  def departmentCount(value: String, dbConnection: Connection): ZIO[Any, Any, Double] = {
    for {
      _ <- printLine(s"Enter the department you want (code) :")
      departmentCodeStr <- readLine.orDie
      departmentName <- ZIO.fromOption(Department.getNameByCode(departmentCodeStr)).orElseFail("Invalid department code")
      departmentCode <- ZIO.fromTry(Try(departmentCodeStr.toInt)).orElseFail("Invalid department code: Not a number")
      count <- countDepartmentStream(departmentCode.toString, mute = true)
      _ <- value match
        case "2" => for {
          dbResultOption <- selectStationsByCode(dbConnection, departmentCode, "DPT")
          _ <- dbResultOption match {
            case Some(dbResult) =>
              val dbCount = dbResult.toDouble
              printLine(s"Data found in DB: Number of stations in $departmentName: ${dbCount.toInt}\n").as(dbCount)
            case None =>
              insertIntoGasStationsByRegDept(dbConnection, count.toInt, departmentCode, "DPT")
          }
        } yield ()
        case _ => for {
          _ <- averagePrice(dbConnection, false, departmentCodeStr, departmentName, count)
        } yield ()
    } yield count
  }

  //Calculate the number of gas stations in a region
  def regionCount(value: String, dbConnection: Connection): ZIO[Any, Any, Double] = {
    for {
      _ <- printLine(s"Enter the region you want (code) :")
      regionCodeStr <- readLine.orDie
      regionName <- ZIO.fromOption(Region.getNameByCode(regionCodeStr)).orElseFail("Invalid region code")
      regionCode <- ZIO.fromTry(Try(regionCodeStr.toInt)).orElseFail("Invalid region code: Not a number")
      count <- countRegionStream(regionCode.toString, mute = true)
      _ <- value match
        case "2" => for {
          dbResultOption <- selectStationsByCode(dbConnection, regionCode, "REG")
          _ <- dbResultOption match {
            case Some(dbResult) =>
              val dbCount = dbResult.toDouble
              printLine(s"Data found in DB: Number of stations in $regionName: ${dbCount.toInt}\n").as(dbCount)
            case None =>
              insertIntoGasStationsByRegDept(dbConnection, count.toInt, regionCode, "REG")
          }
        } yield ()
        case _ => for {
          _ <- averagePrice(dbConnection, true, regionCodeStr, regionName, count)
        } yield ()
    } yield count
  }

  //Calculate the average price of a gas type in a region or department
  def averagePrice(dbConnection: Connection, region: Boolean, code: String, name: String, count: Double): ZIO[Any, Any, Unit] = {
    for{
      _ <- printLine(s"Enter the type of gas (GAZOL, E10, SP98, DIESEL, etc.) :")
      gasTypeStr <- readLine.orDie
      gasTypeOpt = GasType.fromString(gasTypeStr)
      gasType    <- ZIO.fromOption(gasTypeOpt).orElseFail("Invalid GasType")
      _ <- if(region) averagePriceRegion(dbConnection ,code, name, gasType, gasTypeStr, count)
           else averagePriceDepartment(dbConnection, code, name, gasType, gasTypeStr, count)
    } yield ()
  }

  //Calculate the average price of a gas type in a region
  private def averagePriceRegion(dbConnection: Connection, code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Unit] = {
    for {
      dbResultOption <- selectAvgPricesByCode(dbConnection, code.toInt, "REG", gasTypeStr)
      sum <- dbResultOption match {
        case Some(dbResult) =>
          ZIO.succeed(dbResult)
            .tap(dbCount => printLine(s"Data found in DB: Average price of $gasTypeStr in $name: $dbCount\n"))
        case None =>
          averagePriceRegionStream(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double)
            .flatMap { sum =>
              val result = sum / count
              val insertEffect = insertIntoAvgPricesByRegDept(dbConnection, result, code.toInt, "REG", gasTypeStr)
              insertEffect *> printLine("Treatment done.")
            }
      }
    } yield ()
  }

  //Calculate the average price of a gas type in a department
  private def averagePriceDepartment(dbConnection: Connection, code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Unit] = {
    for {
      dbResultOption <- selectAvgPricesByCode(dbConnection, code.toInt, "DPT", gasTypeStr)
      sum <- dbResultOption match {
        case Some(dbResult) =>
          ZIO.succeed(dbResult)
            .tap(dbCount => printLine(s"Data found in DB: The average price of $gasTypeStr in $name: $dbCount\n"))
        case None =>
          averagePriceDepartmentStream(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double)
            .flatMap { sum =>
              val result = sum / count
              val insertEffect = insertIntoAvgPricesByRegDept(dbConnection, result, code.toInt, "DPT", gasTypeStr)
              insertEffect *> printLine("Treatment done.")
            }
      }
    } yield ()
  }

  //Find the type of gas with the highest average price
  def calculateMostExpensiveGas(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      existingRecord <- selectMostExpensiveGas(dbConnection)
      _ <- existingRecord match {
        case Some(gasName:String, price:Double) =>
          printLine(s"Data already exists in DB: The most expensive gas is $gasName with an average price of $price.")
        case None =>
          for {
            mostExpensiveGases <- calculateMostExpensiveGasStream()
            mostExpensiveGas = mostExpensiveGases.headOption
            _ <- mostExpensiveGas match {
              case Some((gasName, price)) =>
                for {
                  _ <- insertMostExpensiveGas(dbConnection, gasName.toString, price)
                } yield ()
              case None =>
                printLine("Issue: no gas types found.")
            }
          } yield ()
      }
    } yield ()
  }

  //Find the 5 most recurring extra services in gas stations
  def calculateMostPresentExtraService(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      _ <- printLine("The 5 most present services in gas stations: ")
      existingServices <- selectMostPresentServices(dbConnection)
      _ <- existingServices match {
        case services if services.isEmpty => //No data in DB
          for {
            extraServicesCount <- findMostPresentExtraServiceStream()
            _ <- insertMostPresentServices(dbConnection, extraServicesCount)
          } yield ()
        case services => //Data found in DB
          ZIO.foreach(services) { case (service, count) =>
            printLine(s"From DB: Extra Service: $service, Count: $count")
          }
      }
    } yield ()
  }

  //Find the department with the most gas stations
  def findDepartmentWithMostGasStations(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      existingRecord <- selectDptMostGasStations(dbConnection)
      _ <- existingRecord match {
        case Some(departmentName:String, count:Int) =>
          printLine(s"Data already exists in DB: $count stations for $departmentName")
        case None =>
          for {
            departmentsWithMostStations <- findDepartmentWithMostGasStationsStream()
            firstElement = departmentsWithMostStations.headOption
            _ <- firstElement match {
              case Some((department, stations)) =>
                val departmentName = department.name
                val nbStations = stations
                for {
                  _ <- insertIntoDptMostGasStations(dbConnection, nbStations, departmentName)
                } yield ()
              case None =>
                printLine("Issue: no gas stations found.")
            }
          } yield ()
      }
    } yield ()
  }

  //Calculate the average number of extra services per station
  def calculateAverageExtraServicesPerStation(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      existingRecord <- selectAverageNumberOfExtraServices(dbConnection)
      _ <- existingRecord match {
        case Some(avg:Double) =>
          printLine(s"Data already exists in DB: The average number of extra services per station is: $avg")
        case None =>
          for {
            averageExtraServices <- calculateAverageExtraServicePerStationStream()
            _ <- insertAverageNumberOfExtraServices(dbConnection, averageExtraServices)
          } yield ()
      }
    } yield ()
  }

  //Calculate the AVERAGE price of all gas types combined in stations where specific extra services are available using mapPar to perform the operation for each extra service in parallel
  //Then sort and print the results in descending order
  def calculateAveragePriceForExtraServices(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      existingRecords <- selectAverageGasPriceForExtraServices(dbConnection)
      _ <- existingRecords match {
        case existingRecords if existingRecords.nonEmpty =>
          ZIO.foreach(existingRecords.sortBy(-_._2)) { case (service, avg) =>
            printLine(s"From DB: Extra Service: $service, Average Price: $avg")
          }
        case _ =>
          for {
            pricesForServices <- calculateAveragePriceForExtraServicesStream()
            _ <- insertAverageGasPriceForExtraServices(dbConnection, pricesForServices.sortBy(-_._2))
          } yield ()
      }

    } yield ()
  }

}
