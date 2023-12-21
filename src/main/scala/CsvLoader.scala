import zio._
import zio.stream._
import zio.Console._
import com.github.tototoshi.csv._

implicit object CustomFormat extends DefaultCSVFormat { // Notre fichier CSV utilise le délimiteur ';' au lieu de ','
  override val delimiter = ';'
}

def loadGasStationCsv():ZIO[Any & ZIOAppArgs & Scope, Throwable, Unit] = {
  for {
    _ <- printLine("Chargement du fichier CSV des stations essence")
    url <- ZIO.succeed(getClass.getClassLoader.getResource("stations.csv"))
    source <- ZIO.succeed(CSVReader.open(url.getFile))
    stream <- ZStream
      .fromIterator[Seq[String]](source.iterator)
      .map[Option[Int]] {
        case line if line.head == "id" => None
        case line => Some(line.head.toInt)
      }
      .collectSome[Int]
      .foreach(Console.printLine(_))
  } yield ()
}