import GasType._
import com.github.tototoshi.csv.CSVReader
import zio.Console.*
import zio.ZIO
import zio.stream.ZSink
import GasStation.*
import zio.stream.*
import zio._


def makeTreatments(): ZIO[Any, Any, Unit] = {
  for {
    _ <- printLine("Reading treatments")
    count <- loadGasStationCsv()
      .filter(_.geographicData.department.code == "75")
      .filter(_.serviceData.automate24)
      .tap(printLine(_))
      .run(ZSink.count)
    sum <- loadGasStationCsv()
      .filter(_.geographicData.department.code == "75")
      .filter(_.serviceData.automate24)
      .map(_.serviceData.gasList(Gazol))
      .map(GasPrice.unapply)
      .collectSome[Double]
      .run(ZSink.sum)
    _ <- printLine(s"Number of matching stations: $count")
    _ <- printLine(s"Average price: ${sum / count}")
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
  } yield ()
}


def calculateMostPresentExtraService(): ZIO[Any, Any, Unit] = {
  for {
    gasStations <- loadGasStationCsv().runCollect // collect all station
    extraServiceList = gasStations.flatMap(_.serviceData.extraService)
    extraService = extraServiceList.groupBy(identity).view.mapValues(_.length)  // count the number of occurence by type
    mostPresentExtraService = extraService.maxByOption(_._2) // find the highest occurence type
    _ <- mostPresentExtraService match {
      case Some((extraService, count)) =>
        printLine(s"The most present extra service : $extraService with a count of $count")
      case None =>
        printLine("Issue : No service find.")
    }
  } yield ()
}



