import GasType._
import com.github.tototoshi.csv.DefaultCSVFormat
import zio.Console.*
import zio.ZIO.*
import zio.stream.*
import zio._

object Treatments{

  implicit object CustomFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  def departmentCount(value: String): ZIO[Any, Any, Double] = {
    for {
      _ <- printLine(s"\nEnter the department you want (code) :")
      departmentCode <- readLine.orDie
      departmentName <- ZIO.fromOption(Department.getNameByCode(departmentCode)).orElseFail("Invalid department code")
      count <- loadGasStationCsv()
        .filter(_.geographicData.department.code == departmentCode)
        .run(ZSink.count)
      _ <- if(value == "1") printLine(s"Number of stations in ${departmentName}: $count\n")
           else averagePrice(false, departmentCode, departmentName, count)
    } yield count
  }

  def regionCount(value: String): ZIO[Any, Any, Double] = {
    for {
      _ <- printLine(s"\nEnter the region you want (code) :")
      regionCode <- readLine.orDie
      regionName <- ZIO.fromOption(Region.getNameByCode(regionCode)).orElseFail("Invalid region code")
      count <- loadGasStationCsv()
        .filter(_.geographicData.region.code == regionCode)
        .run(ZSink.count)
      _ <- if(value == "1") printLine(s"Number of stations in ${regionName}: $count\n") 
           else averagePrice(true, regionCode, regionName, count)
    } yield count
  }

  def averagePrice(region: Boolean, code: String, name: String, count: Double): ZIO[Any, Any, Unit] = {
    for{
      _ <- printLine(s"\nEnter the type of gas (GAZOL, E10, SP98, DIESEL, etc.) :")
      gasTypeStr <- readLine.orDie
      gasTypeOpt = GasType.fromString(gasTypeStr)
      gasType    <- ZIO.fromOption(gasTypeOpt).orElseFail("Invalid GasType")
      _ <- if(region) averagePriceRegion(code, name, gasType, gasTypeStr, count) 
           else averagePriceDepartment(code, name, gasType, gasTypeStr, count)
    } yield ()
  }

  def averagePriceRegion(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Unit] = {
    for{
      sum <- loadGasStationCsv()
        .filter(_.geographicData.region.code == code)
        .filter(_.serviceData.gasList.contains(gasType))
        .map(_.serviceData.gasList(gasType))
        .map(GasPrice.unapply)
        .collectSome[Double]
        .run(ZSink.sum)
      _ <- printLine(s"\nAverage price of ${gasTypeStr} in ${name}: ${sum/count}")
    } yield ()
  }

  def averagePriceDepartment(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Unit] = {
    for{
      sum <- loadGasStationCsv()
        .filter(_.geographicData.department.code == code)
        .filter(_.serviceData.gasList.contains(gasType))
        .map(_.serviceData.gasList(gasType))
        .map(GasPrice.unapply)
        .collectSome[Double]
        .run(ZSink.sum)
      _ <- printLine(s"\nAverage price of ${gasTypeStr} in ${name}: ${sum/count}")
    } yield ()
  }

  def calculateMostExpensiveGas(): ZIO[Any, Any, Unit] = {
    for {
      gasStations <- loadGasStationCsv().runCollect     // collect all the station
      gasList = gasStations.flatMap(_.serviceData.gasList)
      gasPrices = gasList.groupBy(_._1).view.mapValues(_.map(_._2)) // group by type
      avgPrices = gasPrices.mapValues(prices => prices.flatMap(GasPrice.unapply).foldLeft(0.0)(_ + _) / prices.length.toDouble) // calcul price for each type
      mostExpensiveGas = avgPrices.maxByOption(_._2) // find the most expensive
      _ <- mostExpensiveGas match {
        case Some((gas, price)) =>
          printLine(s"The gas type with the highest average price is: $gas with an average price of $price")
        case None =>
          printLine("Issue : No type find.")
      }
      _ <- printLine(" \n ")
    } yield ()
  }

  def calculateMostPresentExtraService(): ZIO[Any, Any, Unit] = {
    for {
      _ <- printLine("The 5 most present services in gas stations : ")
      gasStations <- loadGasStationCsv().runCollect // Collect all stations
      extraServiceList = gasStations.flatMap(_.serviceData.extraService)
      extraService = extraServiceList
        .filterNot(_ == ExtraServices.DomesticGasSales) // exclude DomesticGasSales
        .groupBy(identity)
        .view.mapValues(_.length) // count number of occurence
        .toSeq
        .sortBy(-_._2) // sort by number
        .take(5) // take 5 of them
      _ <- ZIO.foreach(extraService) { case (service, count) =>
        printLine(s"Extra Service: $service, Count: $count")
      }
      _ <- printLine(" \n ")
    } yield ()
  }

  def findDepartmentWithMostGasStations(): ZIO[Any, Any, Unit] = {
    for {
      gasStations <- loadGasStationCsv().runCollect // collect all station
      departmentGasStations = gasStations.groupBy(_.geographicData.department) // group by departement
      departmentWithMostStations = departmentGasStations.maxByOption(_._2.size) // find the highest number of gas station in the departement
      _ <- departmentWithMostStations match {
        case Some((department, stations)) =>
          printLine(s"The department with the most gas stations is: $department with ${stations.size} stations")
        case None =>
          printLine("Issue : no gas stations found.")
      }
      _ <- printLine(" \n ")
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
