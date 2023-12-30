import GasType.*
import zio.Console.*
import zio.ZIO
import zio.stream.{ZSink, ZStream}
import zio.stream.*
import zio._


def makeTreatments(): ZIO[Any, Any, Unit] = {
  for {
    _ <- printLine("Reading treatments")
    count <- loadGasStationCsv()
      .tap(printLine(_))
      .run(ZSink.count)
    _ <- printLine(s"Number of stations: $count")
    sum <- loadGasStationCsv()
      .filter(_.serviceData.gasList.contains(Gazol))
      .map(_.serviceData.gasList(Gazol))
      .map(GasPrice.unapply)
      .collectSome[Double]
      .run(ZSink.sum)
    _ <- printLine(s"Average gazole price: ${sum/count}")
    _ <- printLine("Now displaying data about stations in Paris which have 24h service and Gazol")
    fcount <- applyFilters(loadGasStationCsv())
      .run(ZSink.count)
    fsum <- applyFilters(loadGasStationCsv())
      .filter(_.serviceData.gasList.contains(Gazol))
      .map(_.serviceData.gasList(Gazol))
      .map(GasPrice.unapply)
      .collectSome[Double]
      .run(ZSink.sum)
    _ <- printLine(s"Number of matching stations: $fcount")
    _ <- printLine(s"Average gazole price: ${fsum/fcount}")

    _ <- printLine("Now grouping elements by department code then counting the number of stations in each department")
    _ <- printLine("Outputing the result as a set of (code,number of stations) couples")
    grouped <- loadGasStationCsv()
      .groupByKey(_.geographicData.department.code) {
        case (k, s) => ZStream.fromZIO(s.runCollect.map(l => k -> l.size))
      }
      .run(ZSink.collectAllToSet)
    _ <- printLine(grouped)
    _ <- printLine("\n\n\n")
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

def applyFilters(source : ZStream[Any, Any, GasStation]): ZStream[Any, Any, GasStation] = {
  source
    .filter(_.geographicData.department.code == "75")
    .filter(_.serviceData.automate24)
}

