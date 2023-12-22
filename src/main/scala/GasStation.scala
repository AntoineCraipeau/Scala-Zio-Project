case class GasStation(
                       id: GasStationId,
                       geographicData: GeographicData,
                       serviceData: ServiceData
                     )


opaque type GasStationId = Int
object GasStationId: // GasStationId companion object has to be positive
  def apply(value: Int): GasStationId =
    value

  def safe(value: Int): Option[GasStationId] = value match
    case value if value > 0 => Some(value)
    case _ => None
  def unapply(gasStationId: GasStationId): Option[Int] = Some(gasStationId)

opaque type Address = String
object Address: // GasStationId companion object has to be positive
  def apply(value: String): Address =
    value

  def safe(value: String): Option[Address] = value match
    case value if value.nonEmpty => Some(value)
    case _ => None
  def unapply(address: Address): Option[String] = Some(address)

opaque type City = String
object City: // GasStationId companion object has to be positive
  def apply(value: String): City =
    value
  def safe(value: String): Option[City] = value match
    case value if value.nonEmpty => Some(value)
    case _ => None
  def unapply(city: City): Option[String] = Some(city)

enum Population:
  case Route, Autoroute

case class Coordinates(
                        latitude : Latitude,
                        longitude : Longitude
                      )

opaque type Latitude = Double
object Latitude: // GasStationId companion object has to be positive
  def apply(value: Double): Latitude =
    value
  def safe(value: Double): Option[Latitude] = value match
    case value if value >= -9000000 && value <= 9000000 => Some(value)
    case _ => None
  def unapply(latitude: Latitude): Option[Double] = Some(latitude)

opaque type Longitude = Double
object Longitude: // GasStationId companion object has to be positive
  def apply(value: Double): Longitude =
    value
  def safe(value: Double): Option[Longitude] = value match
    case value if value >= -18000000 && value <= 18000000 => Some(value)
    case _ => None
  def unapply(longitude: Longitude): Option[Double] = Some(longitude)

enum Department(val code: String, val name: String):
  case Ain extends Department(code = "01", name = "Ain")
  case Aisne extends Department(code = "02", name = "Aisne")
  case Allier extends Department(code = "03", name = "Allier")
  case AlpesdeHauteProvence extends Department(code = "04", name = "Alpes-de-Haute-Provence")
  case HautesAlpes extends Department(code = "05", name = "Hautes-Alpes")
  case AlpesMaritimes extends Department(code = "06", name = "Alpes-Maritimes")
  case Ardeche extends Department(code = "07", name = "Ardèche")
  case Ardennes extends Department(code = "08", name = "Ardennes")
  case Ariege extends Department(code = "09", name = "Ariège")
  case Aube extends Department(code = "10", name = "Aube")
  case Aude extends Department(code = "11", name = "Aude")
  case Aveyron extends Department(code = "12", name = "Aveyron")
  case BouchesduRhone extends Department(code = "13", name = "Bouches-du-Rhône")
  case Calvados extends Department(code = "14", name = "Calvados")
  case Cantal extends Department(code = "15", name = "Cantal")
  case Charente extends Department(code = "16", name = "Charente")
  case CharenteMaritime extends Department(code = "17", name = "Charente-Maritime")
  case Cher extends Department(code = "18", name = "Cher")
  case Correze extends Department(code = "19", name = "Corrèze")
  case CorseDuSud extends Department(code = "2A", name = "Corse-du-Sud")
  case HauteCorse extends Department(code = "2B", name = "Haute-Corse")
  case CotedOr extends Department(code = "21", name = "Côte-d'Or")
  case CotesdArmor extends Department(code = "22", name = "Côtes-d'Armor")
  case Creuse extends Department(code = "23", name = "Creuse")
  case Dordogne extends Department(code = "24", name = "Dordogne")
  case Doubs extends Department(code = "25", name = "Doubs")
  case Drome extends Department(code = "26", name = "Drôme")
  case Eure extends Department(code = "27", name = "Eure")
  case EureetLoir extends Department(code = "28", name = "Eure-et-Loir")
  case Finistere extends Department(code = "29", name = "Finistère")
  case Gard extends Department(code = "30", name = "Gard")
  case HauteGaronne extends Department(code = "31", name = "Haute-Garonne")
  case Gers extends Department(code = "32", name = "Gers")
  case Gironde extends Department(code = "33", name = "Gironde")
  case Herault extends Department(code = "34", name = "Hérault")
  case IlleetVilaine extends Department(code = "35", name = "Ille-et-Vilaine")
  case Indre extends Department(code = "36", name = "Indre")
  case IndreetLoire extends Department(code = "37", name = "Indre-et-Loire")
  case Isere extends Department(code = "38", name = "Isère")
  case Jura extends Department(code = "39", name = "Jura")
  case Landes extends Department(code = "40", name = "Landes")
  case LoiretCher extends Department(code = "41", name = "Loir-et-Cher")
  case Loire extends Department(code = "42", name = "Loire")
  case HauteLoire extends Department(code = "43", name = "Haute-Loire")
  case LoireAtlantique extends Department(code = "44", name = "Loire-Atlantique")
  case Loiret extends Department(code = "45", name = "Loiret")
  case Lot extends Department(code = "46", name = "Lot")
  case LotetGaronne extends Department(code = "47", name = "Lot-et-Garonne")
  case Lozere extends Department(code = "48", name = "Lozère")
  case MaineetLoire extends Department(code = "49", name = "Maine-et-Loire")
  case Manche extends Department(code = "50", name = "Manche")
  case Marne extends Department(code = "51", name = "Marne")
  case HauteMarne extends Department(code = "52", name = "Haute-Marne")
  case Mayenne extends Department(code = "53", name = "Mayenne")
  case MeurtheetMoselle extends Department(code = "54", name = "Meurthe-et-Moselle")
  case Meuse extends Department(code = "55", name = "Meuse")
  case Morbihan extends Department(code = "56", name = "Morbihan")
  case Moselle extends Department(code = "57", name = "Moselle")
  case Nievre extends Department(code = "58", name = "Nièvre")
  case Nord extends Department(code = "59", name = "Nord")
  case Oise extends Department(code = "60", name = "Oise")
  case Orne extends Department(code = "61", name = "Orne")
  case PasdeCalais extends Department(code = "62", name = "Pas-de-Calais")
  case PuydeDome extends Department(code = "63", name = "Puy-de-Dôme")
  case PyreneesAtlantiques extends Department(code = "64", name = "Pyrénées-Atlantiques")
  case HautesPyrenees extends Department(code = "65", name = "Hautes-Pyrénées")
  case PyreneesOrientales extends Department(code = "66", name = "Pyrénées-Orientales")
  case BasRhin extends Department(code = "67", name = "Bas-Rhin")
  case HautRhin extends Department(code = "68", name = "Haut-Rhin")
  case Rhone extends Department(code = "69", name = "Rhône")
  case HauteSaone extends Department(code = "70", name = "Haute-Saône")
  case SaoneetLoire extends Department(code = "71", name = "Saône-et-Loire")
  case Sarthe extends Department(code = "72", name = "Sarthe")
  case Savoie extends Department(code = "73", name = "Savoie")
  case HauteSavoie extends Department(code = "74", name = "Haute-Savoie")
  case Paris extends Department(code = "75", name = "Paris")
  case SeineMaritime extends Department(code = "76", name = "Seine-Maritime")
  case SeineetMarne extends Department(code = "77", name = "Seine-et-Marne")
  case Yvelines extends Department(code = "78", name = "Yvelines")
  case DeuxSevres extends Department(code = "79", name = "Deux-Sèvres")
  case Somme extends Department(code = "80", name = "Somme")
  case Tarn extends Department(code = "81", name = "Tarn")
  case TarnetGaronne extends Department(code = "82", name = "Tarn-et-Garonne")
  case Var extends Department(code = "83", name = "Var")
  case Vaucluse extends Department(code = "84", name = "Vaucluse")
  case Vendee extends Department(code = "85", name = "Vendée")
  case Vienne extends Department(code = "86", name = "Vienne")
  case HauteVienne extends Department(code = "87", name = "Haute-Vienne")
  case Vosges extends Department(code = "88", name = "Vosges")
  case Yonne extends Department(code = "89", name = "Yonne")
  case TerritoiredeBelfort extends Department(code = "90", name = "Territoire de Belfort")
  case Essonne extends Department(code = "91", name = "Essonne")
  case HautsdeSeine extends Department(code = "92", name = "Hauts-de-Seine")
  case SeineSaintDenis extends Department(code = "93", name = "Seine-Saint-Denis")
  case ValdeMarne extends Department(code = "94", name = "Val-de-Marne")
  case ValdOise extends Department(code = "95", name = "Val-d'Oise")


enum Region (val code: String, val name: String):
  case AuvergneRhoneAlpes extends Region(code = "84", name = "Auvergne-Rhône-Alpes")
  case BourgogneFrancheComte extends Region(code = "27", name = "Bourgogne-Franche-Comté")
  case Bretagne extends Region(code = "53", name = "Bretagne")
  case CentreValdeLoire extends Region(code = "24", name = "Centre-Val de Loire")
  case Corse extends Region(code = "94", name = "Corse")
  case GrandEst extends Region(code = "44", name = "Grand Est")
  case HautsdeFrance extends Region(code = "32", name = "Hauts-de-France")
  case IledeFrance extends Region(code = "11", name = "Île-de-France")
  case Normandie extends Region(code = "28", name = "Normandie")
  case NouvelleAquitaine extends Region(code = "75", name = "Nouvelle-Aquitaine")
  case Occitanie extends Region(code = "76", name = "Occitanie")
  case PaysdelaLoire extends Region(code = "52", name = "Pays de la Loire")
  case ProvenceAlpesCotedAzur extends Region(code = "93", name = "Provence-Alpes-Côte d'Azur")

case class GeographicData(
                           population: Population,
                           address: Address,
                           city: City,
                           region: Region,
                           department: Department,
                           coordinates: Coordinates
                         )



opaque type GasPrice = Double
object GasPrice: // GasStationId companion object has to be positive
  def apply(value: Double): GasPrice =
    value
  def safe(value: Double): Option[GasPrice] = value match
    case value if value > 0 => Some(value)
    case _ => None
  def unapply(gasPrice: GasPrice): Option[Double] = Some(gasPrice)


enum GasType:
  case SP98, SP95, Gazol, E10, Diesel, E85, GPLc


enum ExtraServices:
  case PublicToilets, Laundry, ParcelRelay, FoodShop, NonFoodShop, TakeAwayFood, SitInRestaurant, Bar, LampOilSales, InflationStation, AdditiveFuel, VehicleRental, HeavyVehicleLane, AutomaticCarWash, ManualCarWash, DomesticGasSales, DomesticFuelSales, Wifi, ATM24_7CashMachine, CashDispenser, BabyArea, ElectricalTerminals, RepairMaintenanceServices, Showers, FuelAdditivesSales, GNV, CampingCarArea

case class ServiceData(
                        gasList: Map[GasType, GasPrice],
                        extraService: List[ExtraServices],
                        automate24: Boolean
                      )
