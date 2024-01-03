import zio.*
import zio.stream.*
import com.github.tototoshi.csv.*
import GasStation.*
import GeographicData.*
import ServiceData.*
import GasType.*
import ExtraServices.*
import Region.*
import Department.*
import GasStationId.*
import Population.*
import Address.*
import City.*
import Coordinates.*
import Latitude.*
import Longitude.*
import GasPrice.*

import scala.util.Try
import java.io.InputStream
import java.io.InputStreamReader


def loadGasStationCsv(): ZStream[Any, Any, GasStation]= {
  val url: java.net.URL = Main.getClass.getClassLoader.getResource("stations.csv")
  val inputStream: InputStream = url.openStream()
  val reader: java.io.Reader = new InputStreamReader(inputStream)
  val source = CSVReader.open(reader)

  ZStream
    .fromIterator[Seq[String]](source.iterator)
    .map[Option[GasStation]] {
      case line if line.head == "id" => None
      case lId :: lLatitude :: lLongitude :: _ :: lPop :: lAddress :: lCity :: _ :: _ :: _ :: _ :: _ :: lGazolePrice :: _ :: lSp95Price :: _ :: lE85Price :: _ :: lGplPrice :: _ :: lE10Price :: _ :: lSp98Price :: _ :: _ :: lAutomate24 :: lServices :: _ :: lDepartmentCode :: _ :: lRegionCode =>

        val gasStationId = toIntOption(lId)
        val latitude = toDoubleOption(lLatitude)
        val longitude = toDoubleOption(lLongitude)
        val department = Try(parseDepartmentCode(lDepartmentCode)).toOption
        val region = Try(parseRegionCode(lRegionCode.head)).toOption
        val services = Try(parseServices(lServices)).toOption
        val population = Try(parsePop(lPop)).toOption
        val gasList = Try(parseGas(lGazolePrice, lSp95Price, lE85Price, lGplPrice, lE10Price, lSp98Price)).toOption
        val address = Address.safe(lAddress)
        val city = City.safe(lCity)
        val automate24 = Try(parseBool(lAutomate24)).toOption

        for {
          lid <- gasStationId
          lat <- latitude
          lon <- longitude
          dpt <- department
          reg <- region
          srv <- services
          pop <- population
          addr <- address
          city <- city
          gas <- gasList
          a24 <- automate24

        } yield GasStation(
          id = GasStationId.apply(lid),
          geographicData = GeographicData(
            population = pop,
            address = addr,
            city = city,
            region = reg,
            department = dpt,
            coordinates = Coordinates(Latitude.apply(lat), Longitude.apply(lon)),
          ),
          serviceData = ServiceData(
            gasList = gas,
            extraService = srv,
            automate24 = a24,
          ),
        )
      case _ => None
    }
    .collectSome[GasStation]
}

def parsePop(s: String): Population =
  s match {
    case "A" => Population.Autoroute
    case "R" => Population.Route
    case _ => throw new Exception("Invalid population")
  }

def parseServices(s: String): List[ExtraServices] =
  s.replace("Butane,","Butane &").split(",").filter(s => s != "").map(parseOneService).toList

def parseOneService(s: String): ExtraServices =
  s match {
    case "Toilettes publiques" => ExtraServices.PublicToilets
    case "Laverie" => ExtraServices.Laundry
    case "Relais colis" => ExtraServices.ParcelRelay
    case "Boutique alimentaire" => ExtraServices.FoodShop
    case "Boutique non alimentaire" => ExtraServices.NonFoodShop
    case "Restauration à emporter" => ExtraServices.TakeAwayFood
    case "Restauration sur place" => ExtraServices.SitInRestaurant
    case "Bar" => ExtraServices.Bar
    case "Vente de pétrole lampant" => ExtraServices.LampOilSales
    case "Station de gonflage" => ExtraServices.InflationStation
    case "Carburant additivé" => ExtraServices.AdditiveFuel
    case "Location de véhicule" => ExtraServices.VehicleRental
    case "Piste poids lourds" => ExtraServices.HeavyVehicleLane
    case "Lavage automatique" => ExtraServices.AutomaticCarWash
    case "Lavage manuel" => ExtraServices.ManualCarWash
    case "Vente de gaz domestique (Butane & Propane)" => ExtraServices.DomesticGasSales
    case "Vente de fioul domestique" => ExtraServices.DomesticFuelSales
    case "Wifi" => ExtraServices.Wifi
    case "Automate CB 24/24" => ExtraServices.ATM24_7CashMachine
    case "DAB (Distributeur automatique de billets)" => ExtraServices.CashDispenser
    case "Espace bébé" => ExtraServices.BabyArea
    case "Bornes électriques" => ExtraServices.ElectricalTerminals
    case "Services réparation / entretien" => ExtraServices.RepairMaintenanceServices
    case "Douches" => ExtraServices.Showers
    case "Vente d'additifs carburants" => ExtraServices.FuelAdditivesSales
    case "GNV" => ExtraServices.GNV
    case "Aire de camping-cars" => ExtraServices.CampingCarArea
    case _ => throw new Exception(s"Invalid service: $s")
  }

def parseBool(s: String): Boolean =
  s match {
    case "Oui" => true
    case "Non" => false
    case _ => throw new Exception("Invalid boolean")
  }

def parseRegionCode(s: String): Region =
  s match{
    case "84" => Region.AuvergneRhoneAlpes
    case "27" => Region.BourgogneFrancheComte
    case "53" => Region.Bretagne
    case "24" => Region.CentreValdeLoire
    case "94" => Region.Corse
    case "44" => Region.GrandEst
    case "32" => Region.HautsdeFrance
    case "11" => Region.IledeFrance
    case "28" => Region.Normandie
    case "75" => Region.NouvelleAquitaine
    case "76" => Region.Occitanie
    case "52" => Region.PaysdelaLoire
    case "93" => Region.ProvenceAlpesCotedAzur
    case _ => throw new Exception(s"Invalid region: $s")
  }

def parseDepartmentCode(s: String): Department =
  s match {
    case "01" => Department.Ain
    case "02" => Department.Aisne
    case "03" => Department.Allier
    case "04" => Department.AlpesdeHauteProvence
    case "05" => Department.HautesAlpes
    case "06" => Department.AlpesMaritimes
    case "07" => Department.Ardeche
    case "08" => Department.Ardennes
    case "09" => Department.Ariege
    case "10" => Department.Aube
    case "11" => Department.Aude
    case "12" => Department.Aveyron
    case "13" => Department.BouchesduRhone
    case "14" => Department.Calvados
    case "15" => Department.Cantal
    case "16" => Department.Charente
    case "17" => Department.CharenteMaritime
    case "18" => Department.Cher
    case "19" => Department.Correze
    case "2A" => Department.CorseDuSud
    case "2B" => Department.HauteCorse
    case "21" => Department.CotedOr
    case "22" => Department.CotesdArmor
    case "23" => Department.Creuse
    case "24" => Department.Dordogne
    case "25" => Department.Doubs
    case "26" => Department.Drome
    case "27" => Department.Eure
    case "28" => Department.EureetLoir
    case "29" => Department.Finistere
    case "30" => Department.Gard
    case "31" => Department.HauteGaronne
    case "32" => Department.Gers
    case "33" => Department.Gironde
    case "34" => Department.Herault
    case "35" => Department.IlleetVilaine
    case "36" => Department.Indre
    case "37" => Department.IndreetLoire
    case "38" => Department.Isere
    case "39" => Department.Jura
    case "40" => Department.Landes
    case "41" => Department.LoiretCher
    case "42" => Department.Loire
    case "43" => Department.HauteLoire
    case "44" => Department.LoireAtlantique
    case "45" => Department.Loiret
    case "46" => Department.Lot
    case "47" => Department.LotetGaronne
    case "48" => Department.Lozere
    case "49" => Department.MaineetLoire
    case "50" => Department.Manche
    case "51" => Department.Marne
    case "52" => Department.HauteMarne
    case "53" => Department.Mayenne
    case "54" => Department.MeurtheetMoselle
    case "55" => Department.Meuse
    case "56" => Department.Morbihan
    case "57" => Department.Moselle
    case "58" => Department.Nievre
    case "59" => Department.Nord
    case "60" => Department.Oise
    case "61" => Department.Orne
    case "62" => Department.PasdeCalais
    case "63" => Department.PuydeDome
    case "64" => Department.PyreneesAtlantiques
    case "65" => Department.HautesPyrenees
    case "66" => Department.PyreneesOrientales
    case "67" => Department.BasRhin
    case "68" => Department.HautRhin
    case "69" => Department.Rhone
    case "70" => Department.HauteSaone
    case "71" => Department.SaoneetLoire
    case "72" => Department.Sarthe
    case "73" => Department.Savoie
    case "74" => Department.HauteSavoie
    case "75" => Department.Paris
    case "76" => Department.SeineMaritime
    case "77" => Department.SeineetMarne
    case "78" => Department.Yvelines
    case "79" => Department.DeuxSevres
    case "80" => Department.Somme
    case "81" => Department.Tarn
    case "82" => Department.TarnetGaronne
    case "83" => Department.Var
    case "84" => Department.Vaucluse
    case "85" => Department.Vendee
    case "86" => Department.Vienne
    case "87" => Department.HauteVienne
    case "88" => Department.Vosges
    case "89" => Department.Yonne
    case "90" => Department.TerritoiredeBelfort
    case "91" => Department.Essonne
    case "92" => Department.HautsdeSeine
    case "93" => Department.SeineSaintDenis
    case "94" => Department.ValdeMarne
    case "95" => Department.ValdOise
    case _ => throw new Exception(s"Invalid department: $s")
  }

def parseGas(gazole: String, sp95: String, e85: String, gpl: String, e10: String, sp98: String): Map[GasType,GasPrice] = {
  val gasMap = Map[GasType,String](
    GasType.Gazol -> gazole,
    GasType.SP95 -> sp95,
    GasType.E85 -> e85,
    GasType.GPLc -> gpl,
    GasType.E10 -> e10,
    GasType.SP98 -> sp98,
  )
  gasMap.filter((_,v) => v != "").map((k,v) => (k,GasPrice(v.toDouble)))
}

def toIntOption(s: String): Option[Int] =
  s match {
    case "" => None
    case _ => Some(s.toInt)
  }

def toDoubleOption(s: String): Option[Double] =
  s match {
    case "" => None
    case _ => Some(s.toDouble)
  }