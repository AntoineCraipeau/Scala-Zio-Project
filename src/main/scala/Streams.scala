import com.github.tototoshi.csv.DefaultCSVFormat
import zio.Console.printLine
import zio.ZIO
import zio.stream.ZSink

object Streams{

  implicit object CustomFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  def averagePriceDepartmentStream(code: String, name: String, gasType: GasType, gasTypeStr: String, count: Double): ZIO[Any, Any, Double] = {
    for{
      sum <- loadGasStationCsv()
        .filter(_.geographicData.department.code == code)
        .filter(_.serviceData.gasList.contains(gasType))
        .map(_.serviceData.gasList(gasType))
        .map(GasPrice.unapply)
        .collectSome[Double]
        .run(ZSink.sum)
      _ <- printLine(s"\nAverage price of ${gasTypeStr} in ${name}: ${sum / count}")
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

  def countDepartmentStream(departmentCode: String): ZIO[Any, Any, Double] = {
    for{
      count <- loadGasStationCsv()
        .filter(_.geographicData.department.code == departmentCode)
        .run(ZSink.count)
      _ <- printLine(s"\nNumber of gas stations in ${departmentCode}: ${count}")
    }yield count
  }

  def countRegionStream(regionCode: String): ZIO[Any, Any, Double] = {
    for{
      count <- loadGasStationCsv()
        .filter(_.geographicData.region.code == regionCode)
        .run(ZSink.count)
      _ <- printLine(s"\nNumber of gas stations in ${regionCode}: ${count}")
    }yield count
  }

  //Same but returns the sequence of most present extra services with their count
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