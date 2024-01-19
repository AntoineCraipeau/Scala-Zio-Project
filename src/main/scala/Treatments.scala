import GasType._
import GasType.*
import Streams.*
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.DefaultCSVFormat
import zio.Console.*
import zio.ZIO.*
import zio.stream.*
import zio._
import zio.{ZIO, *}
import zio.Config.Bool

import java.sql.Connection
import java.sql.Statement
import scala.util.Try

object Treatments{

  implicit object CustomFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  def departmentCount(value: String, dbConnection: Connection): ZIO[Any, Any, Double] = {
    for {
      _ <- printLine(s"\nEnter the department you want (code) :")
      departmentCodeStr <- readLine.orDie
      departmentName <- ZIO.fromOption(Department.getNameByCode(departmentCodeStr)).orElseFail("Invalid department code")
      departmentCode <- ZIO.fromTry(Try(departmentCodeStr.toInt)).orElseFail("Invalid department code: Not a number")
      dbResultOption <- selectStationsByCode(dbConnection, departmentCode, "DPT")
      count <- dbResultOption match {
        case Some(dbResult) =>
          val dbCount = dbResult.toDouble
          ZIO.succeed(dbCount)
            .tap(_ => printLine(s"Data found in DB: Number of stations in ${departmentName}: ${dbCount.toInt}\n"))
            .flatMap { _ =>
              if (value != "2") {
                averagePrice(dbConnection, false, departmentCodeStr, departmentName, dbCount).as(dbCount)
              } else {
                ZIO.succeed(dbCount)
              }
            }
        case None =>
          countDepartmentStream(departmentCode.toString)
            .flatMap { count =>
              val insertEffect = insertIntoGasStationsByRegDept(dbConnection, count.toInt, departmentCode, "DPT")
              insertEffect *> (if (value == "2") {
                printLine(s"Number of stations in ${departmentName}: $count\n").as(count.toDouble)
              } else {
                averagePrice(dbConnection,false , departmentCodeStr, departmentName, count).as(count.toDouble)
              })
            }
      }
    } yield count
  }


  def regionCount(value: String, dbConnection: Connection): ZIO[Any, Any, Double] = {
    for {
      _ <- printLine(s"\nEnter the region you want (code) :")
      regionCodeStr <- readLine.orDie
      regionName <- ZIO.fromOption(Region.getNameByCode(regionCodeStr)).orElseFail("Invalid region code")
      regionCode <- ZIO.fromTry(Try(regionCodeStr.toInt)).orElseFail("Invalid region code: Not a number")
      dbResultOption <- selectStationsByCode(dbConnection, regionCode, "REG")
      count <- dbResultOption match {
        case Some(dbResult) =>
          val dbCount = dbResult.toDouble
          ZIO.succeed(dbCount)
            .tap(_ => printLine(s"Data found in DB: Number of stations in ${regionName}: ${dbCount.toInt}\n"))
            .flatMap { _ =>
              if (value != "2") {
                averagePrice(dbConnection, true, regionCodeStr, regionName, dbCount).as(dbCount)
              } else {
                ZIO.succeed(dbCount)
              }
            }
        case None =>
          countRegionStream(regionCode.toString)
            .flatMap { count =>
              val insertEffect = insertIntoGasStationsByRegDept(dbConnection, count.toInt, regionCode, "REG")
              insertEffect *> (if (value == "2") {
                printLine(s"Number of stations in ${regionName}: $count\n").as(count.toDouble)
              } else {
                averagePrice(dbConnection, true, regionCodeStr, regionName, count).as(count.toDouble)
              })
            }
      }
    } yield count
  }


  def averagePrice(dbConnection: Connection, region: Boolean, code: String, name: String, count: Double): ZIO[Any, Any, Unit] = {
    for{
      _ <- printLine(s"\nEnter the type of gas (GAZOL, E10, SP98, DIESEL, etc.) :")
      gasTypeStr <- readLine.orDie
      gasTypeOpt = GasType.fromString(gasTypeStr)
      gasType    <- ZIO.fromOption(gasTypeOpt).orElseFail("Invalid GasType")
      _ <- if(region) averagePriceRegion(dbConnection ,code, name, gasType, gasTypeStr, count)
           else averagePriceDepartment(dbConnection, code, name, gasType, gasTypeStr, count)
    } yield ()
  }

  def averagePriceRegion(dbConnection: Connection, code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Unit] = {
    for {
      dbResultOption <- selectAvgPricesByCode(dbConnection, code.toInt, "REG", gasTypeStr)
      sum <- dbResultOption match {
        case Some(dbResult) =>
          ZIO.succeed(dbResult)
            .tap(dbCount => printLine(s"Data found in DB: Average price of ${gasTypeStr} in ${name}: ${dbCount}\n"))
        case None =>
          averagePriceRegionStream(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double)
            .flatMap { sum =>
              val result = sum / count
              val insertEffect = insertIntoAvgPricesByRegDept(dbConnection, result, code.toInt, "REG", gasTypeStr)
              insertEffect *> printLine("Treatment done.")
            }
      }
    } yield sum
  }

  def averagePriceDepartment(dbConnection: Connection, code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Unit] = {
    for {
      dbResultOption <- selectAvgPricesByCode(dbConnection, code.toInt, "DPT", gasTypeStr)
      sum <- dbResultOption match {
        case Some(dbResult) =>
          ZIO.succeed(dbResult)
            .tap(dbCount => printLine(s"Data found in DB: The average price of ${gasTypeStr} in ${name}: ${dbCount}\n"))
        case None =>
          averagePriceDepartmentStream(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double)
            .flatMap { sum =>
              val result = sum / count
              val insertEffect = insertIntoAvgPricesByRegDept(dbConnection, result, code.toInt, "DPT", gasTypeStr)
              insertEffect *> printLine("Treatment done.")
            }
      }
    } yield sum
  }

  def calculateMostExpensiveGas(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      gasStations <- loadGasStationCsv().runCollect
      gasList = gasStations.flatMap(_.serviceData.gasList)
      gasPrices = gasList.groupBy(_._1).view.mapValues(_.map(_._2))
      avgPrices = gasPrices.mapValues(prices => prices.flatMap(GasPrice.unapply).foldLeft(0.0)(_ + _) / prices.length.toDouble)
      mostExpensiveGas = avgPrices.maxByOption(_._2)
      _ <- mostExpensiveGas match {
        case Some((gasName, price)) =>
          for {
            existingRecord <- selectMostExpensiveGas(dbConnection)
            _ <- existingRecord match {
              case Some(_) =>
                printLine(s"Data already exists in DB: The most expensive gas is $gasName with an average price of $price.")
              case None =>
                for {
                  _ <- insertMostExpensiveGas(dbConnection, gasName.toString, price)
                } yield ()
            }
          } yield ()
        case None =>
          printLine("Issue: no gas types found.")
      }
    } yield ()
  }



  def calculateMostPresentExtraService(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      _ <- printLine("The 5 most present services in gas stations: ")
      existingServices <- selectMostPresentServices(dbConnection)
      _ <- existingServices match {
        case services if services.isEmpty =>
          for {
            gasStations <- loadGasStationCsv().runCollect
            extraServiceList = gasStations.flatMap(_.serviceData.extraService)
            extraServiceCount = extraServiceList
              .filterNot(_ == ExtraServices.DomesticGasSales)
              .groupBy(identity)
              .view.mapValues(_.length)
              .toSeq
              .sortBy(-_._2)
              .take(5)
            _ <- insertMostPresentServices(dbConnection, extraServiceCount)
            _ <- ZIO.foreach(extraServiceCount) { case (service, count) =>
              printLine(s"Extra Service: $service, Count: $count")
            }
          } yield ()
        case services =>
          ZIO.foreach(services) { case (service, count) =>
            printLine(s"From DB: Extra Service: $service, Count: $count")
          }
      }
    } yield ()
  }


  def findDepartmentWithMostGasStations(dbConnection: Connection): ZIO[Any, Any, Unit] = {
    for {
      gasStations <- loadGasStationCsv().runCollect
      departmentGasStations = gasStations.groupBy(_.geographicData.department)
      departmentWithMostStations = departmentGasStations.maxByOption(_._2.size)
      _ <- departmentWithMostStations match {
        case Some((department, stations)) =>
          val departmentName = department.name
          val nbStations = stations.size
          for {
            existingRecord <- selectDptMostGasStations(dbConnection)
            _ <- existingRecord match {
              case Some(_) =>
                printLine(s"Data already exists in DB: $departmentName with $nbStations stations.")
              case None =>
                for {
                  _ <- insertIntoDptMostGasStations(dbConnection, nbStations, departmentName)
                } yield ()
            }
          } yield ()
        case None =>
          printLine("Issue: no gas stations found.")
      }
    } yield ()
  }

  def calculateAverageExtraServicesPerStation(): ZIO[Any, Any, Unit] = {
    for {
      gasStations <- loadGasStationCsv().runCollect // collect all station
      totalExtraServices = gasStations.flatMap(_.serviceData.extraService.toList).size // count number of service
      averageExtraServices = if (gasStations.nonEmpty) totalExtraServices.toDouble / gasStations.size else 0 // calcul the average
      _ <- printLine(s"The average number of extra services per station is: $averageExtraServices")
      _ <- printLine(" \n ")
    } yield ()
  }

  //Calculate the AVERAGE price of all gas types combined in stations where specific extra services are available using mapPar to perform the operation for each extra service in parallel
  //Then sort and print the results in descending order
  def calculateAveragePriceForExtraServicesWithZStream(): ZIO[Any, Any, Unit] = {
    for {
      gasStations <- loadGasStationCsv().runCollect // collect all station
      extraServices = gasStations.flatMap(_.serviceData.extraService.toList).distinct // get all extra services
      averagePrices <- ZStream.fromIterable(extraServices)
        .mapZIOPar(extraServices.size) { extraService =>
          val gasStationsWithExtraService = gasStations.filter(_.serviceData.extraService.contains(extraService)) // filter station with the extra service
          val gasList = gasStationsWithExtraService.flatMap(_.serviceData.gasList) // get all gas type
          val gasPrices = gasList.groupBy(_._1).view.mapValues(_.map(_._2)) // group by type
          val avgPrices = gasPrices.mapValues(prices => prices.flatMap(GasPrice.unapply).foldLeft(0.0)(_ + _) / prices.length.toDouble) // calcul price for each type
          val avgPrice = avgPrices.values.sum / avgPrices.size // calcul average price
          ZIO.succeed((extraService, avgPrice))
        }
        .run(ZSink.collectAll)
      averagePricesSorted = averagePrices.sortBy(-_._2) // sort by number
      _ <- ZIO.foreach(averagePricesSorted) { case (service, price) =>
        printLine(s"Extra Service: $service => Average Price: $price")
      }
      _ <- printLine(" \n ")
    } yield ()
  }

}
