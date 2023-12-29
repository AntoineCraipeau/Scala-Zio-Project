import GasType._
import com.github.tototoshi.csv.CSVReader
import zio.Console.*
import zio.ZIO
import zio.stream.ZSink
import GasStation.*

def makeTreatments(): ZIO[Any, Any, Unit] ={
  for{
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
    _ <- printLine(s"Average price: ${sum/count}")
  } yield ()
}
