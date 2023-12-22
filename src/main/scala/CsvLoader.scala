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
      .map[Option[GasStation]] {
        case line if line.head == "id" => None
        case line => Some(GasStation(
          id = line.head.toInt,
          population = parsePop(line(4)),
          address = line(5),
          city = line(6),
          schedule = Some(line(7)),
          service = parseServices(line(26)),
          automate24 = parseBool(line(25)),
          region = Location.apply(line(30), line(29)),
          department = Location.apply(line(28), line(27)),
          gasList = parseGas(line(12), line(14), line(16), line(18), line(20), line(22)),
        ))
      }
      .collectSome[GasStation]
      .foreach(Console.printLine(_))
  } yield ()
}

def parsePop(s: String): Population =
  s match {
    case "A" => Population.Autoroute
    case "R" => Population.Route
    case _ => throw new Exception("Invalid population")
  }

def parseServices(s: String): List[Services] =
  List[Services]()

def parseBool(s: String): Boolean =
  s match {
    case "Oui" => true
    case "Non" => false
    case _ => throw new Exception("Invalid boolean")
  }

def parseGas(gazole: String, sp95: String, e85: String, gpl: String, e10: String, sp98: String): List[Gas] =
  List[Gas](
    //If the field is empty, we set price to 0
    Gas(GasType.Gazol, !gazole.equals(""), if !gazole.equals("") then gazole.toDouble else 0),
    Gas(GasType.SP95, !sp95.equals(""), if !sp95.equals("") then sp95.toDouble else 0),
    Gas(GasType.E85, !e85.equals(""), if !e85.equals("") then e85.toDouble else 0),
    Gas(GasType.GPLc, !gpl.equals(""), if !gpl.equals("") then gpl.toDouble else 0),
    Gas(GasType.E10, !e10.equals(""), if !e10.equals("") then e10.toDouble else 0),
    Gas(GasType.SP98, !sp98.equals(""), if !sp98.equals("") then sp98.toDouble else 0),
  )