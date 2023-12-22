import zio._
import zio.stream._
import zio.Console._
import com.github.tototoshi.csv._

implicit object CustomFormat extends DefaultCSVFormat { // Notre fichier CSV utilise le délimiteur ';' au lieu de ','
  override val delimiter = ';'
}

object App extends ZIOAppDefault {

  override def run: ZIO[Any & (ZIOAppArgs & Scope), Any, Unit] =
    for {
      _ <- printLine("Chargement du fichier CSV des stations essence")
      url <- ZIO.succeed(getClass.getClassLoader.getResource("stations.csv"))
      source <- ZIO.succeed(CSVReader.open(url.getFile))
      stream <- loadGasStationCsv(source).foreach(printLine(_))
    } yield ()

}