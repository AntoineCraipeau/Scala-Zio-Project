import zio._
import zio.stream._
import zio.Console._
import com.github.tototoshi.csv._

def loadGasStationCsv(source: CSVReader): ZStream[Any, Any, GasStation]= {
  ZStream
    .fromIterator[Seq[String]](source.iterator)
    .map[Option[GasStation]] {
      case line if line.head == "id" => None
      case line => Some(GasStation(
        id = line.head.toInt,
        population = parsePop(line(4)),
        address = line(5),
        city = line(6),
        schedule = Some("Thinking about it"),
        service = parseServices(line(26)),
        automate24 = parseBool(line(25)),
        region = Location(line(30), line(29)),
        department = Location(line(28), line(27)),
        gasList = parseGas(line(12), line(14), line(16), line(18), line(20), line(22)),
        latitude = line(1).toDouble,
        longitude = line(2).toDouble,
      ))
    }
    .collectSome[GasStation]
}

def parsePop(s: String): Population =
  s match {
    case "A" => Population.Autoroute
    case "R" => Population.Route
    case _ => throw new Exception("Invalid population")
  }

def parseServices(s: String): List[Services] =
  s.replace("Butane,","Butane &").split(",").filter(s => s != "").map(parseOneService).toList

def parseOneService(s: String): Services =
  s match {
    case "Toilettes publiques" => Services.PublicToilets
    case "Laverie" => Services.Laundry
    case "Relais colis" => Services.ParcelRelay
    case "Boutique alimentaire" => Services.FoodShop
    case "Boutique non alimentaire" => Services.NonFoodShop
    case "Restauration à emporter" => Services.TakeAwayFood
    case "Restauration sur place" => Services.SitInRestaurant
    case "Bar" => Services.Bar
    case "Vente de pétrole lampant" => Services.LampOilSales
    case "Station de gonflage" => Services.InflationStation
    case "Carburant additivé" => Services.AdditiveFuel
    case "Location de véhicule" => Services.VehicleRental
    case "Piste poids lourds" => Services.HeavyVehicleLane
    case "Lavage automatique" => Services.AutomaticCarWash
    case "Lavage manuel" => Services.ManualCarWash
    case "Vente de gaz domestique (Butane & Propane)" => Services.DomesticGasSales
    case "Vente de fioul domestique" => Services.DomesticFuelSales
    case "Wifi" => Services.Wifi
    case "Automate CB 24/24" => Services.ATM24_7CashMachine
    case "DAB (Distributeur automatique de billets)" => Services.CashDispenser
    case "Espace bébé" => Services.BabyArea
    case "Bornes électriques" => Services.ElectricalTerminals
    case "Services réparation / entretien" => Services.RepairMaintenanceServices
    case "Douches" => Services.Showers
    case "Vente d'additifs carburants" => Services.FuelAdditivesSales
    case "GNV" => Services.GNV
    case "Aire de camping-cars" => Services.CampingCarArea
    case _ => throw new Exception(s"Invalid service: $s")
  }

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