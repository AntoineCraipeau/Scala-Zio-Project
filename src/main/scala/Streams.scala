import zio.Console.printLine
import zio.ZIO
import zio.stream.{ZSink, ZStream}

object Streams{

  def averagePriceDepartmentStream(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Double] = {
    for{
      sum <- loadGasStationCsv()
        .filter(_.geographicData.department.code == code)
        .filter(_.serviceData.gasList.contains(gasType))
        .map(_.serviceData.gasList(gasType))
        .map(GasPrice.unapply)
        .collectSome[Double]
        .run(ZSink.sum)
      _ <- printLine(s"\nAverage price of $gasTypeStr in $name: ${sum / count}")
    }yield sum
  }

  def averagePriceRegionStream(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Double] = {
    for {
      sum <- loadGasStationCsv()
        .filter(_.geographicData.region.code == code)
        .filter(_.serviceData.gasList.contains(gasType))
        .map(_.serviceData.gasList(gasType))
        .map(GasPrice.unapply)
        .collectSome[Double]
        .run(ZSink.sum)
      _ <- printLine(s"\nAverage price of $gasTypeStr in $name is ${sum / count}")
    } yield sum
  }

  def calculateAverageExtraServicePerStationStream(): ZIO[Any, Any, Double] = {
    for {
      gasStations <- loadGasStationCsv().runCollect // collect all station
      totalExtraServices = gasStations.flatMap(_.serviceData.extraService.toList).size // count number of service
      averageExtraServices = if (gasStations.nonEmpty) totalExtraServices.toDouble / gasStations.size else 0 // calcul the average
      _ <- printLine(s"\nThe average number of extra services per station is: $averageExtraServices")
    } yield averageExtraServices
  }

  def calculateAveragePriceForExtraServicesStream(): ZIO[Any, Any, Seq[(ExtraServices, Double)]] = {
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
      _ <- ZIO.foreachDiscard(averagePrices.sortBy(-_._2))(averagePrice => printLine(s"Extra Service: ${averagePrice._1} => Average Fuel Price: ${averagePrice._2}"))
    } yield averagePrices
  }

  def calculateMostExpensiveGasStream(): ZIO[Any, Any, Seq[(GasType, Double)]] = {
    for {
      gasStations <- loadGasStationCsv().runCollect // collect all station
      gasList = gasStations.flatMap(_.serviceData.gasList) // get all gas type
      gasPrices = gasList.groupBy(_._1).view.mapValues(_.map(_._2)) // group by type
      avgPrices = gasPrices.mapValues(prices => prices.flatMap(GasPrice.unapply).foldLeft(0.0)(_ + _) / prices.length.toDouble) // calcul average price for each type
      mostExpensiveGases = avgPrices.toSeq.sortBy(-_._2).take(5) // sort by price and take 5 most expensive
      _ <- ZIO.foreachDiscard(mostExpensiveGases)(gas => printLine(s"Gas: ${gas._1} => Average Fuel Price: ${gas._2}"))
    } yield mostExpensiveGases
  }

  def countDepartmentStream(departmentCode: String): ZIO[Any, Any, Double] = {
    for{
      count <- loadGasStationCsv()
        .filter(_.geographicData.department.code == departmentCode)
        .run(ZSink.count)
      _ <- printLine(s"\nNumber of gas stations in $departmentCode: $count")
    }yield count
  }

  def countRegionStream(regionCode: String): ZIO[Any, Any, Double] = {
    for{
      count <- loadGasStationCsv()
        .filter(_.geographicData.region.code == regionCode)
        .run(ZSink.count)
      _ <- printLine(s"\nNumber of gas stations in $regionCode: $count")
    }yield count
  }

  def findMostPresentExtraServiceStream(): ZIO[Any, Any, Seq[(ExtraServices, Int)]] = {
    for{
      gasStations <- loadGasStationCsv().runCollect
      extraServiceList = gasStations.flatMap(_.serviceData.extraService)
      extraServiceCount = extraServiceList
        .filterNot(_ == ExtraServices.DomesticGasSales)
        .groupBy(identity)
        .view.mapValues(_.length)
        .toSeq
        .sortBy(-_._2)
        .take(5)
      _ <- ZIO.foreachDiscard(extraServiceCount)(extraService => printLine(s"${extraService._1} with ${extraService._2} stations"))
    }yield extraServiceCount
  }

  def findDepartmentWithMostGasStationsStream(): ZIO[Any, Any, Seq[(Department, Int)]] = {
    for{
      gasStations <- loadGasStationCsv().runCollect
      departmentList = gasStations.map(_.geographicData.department)
      departmentCount = departmentList
        .groupBy(identity)
        .view.mapValues(_.length)
        .toSeq
        .sortBy(-_._2)
        .take(5)
      _ <- ZIO.foreachDiscard(departmentCount)(department => printLine(s"${department._1.name} with ${department._2} stations"))
    }yield departmentCount
  }

}