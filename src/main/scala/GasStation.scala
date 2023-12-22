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

enum Department:
  case Ain (code: String = "01", name: String = "Ain")
  case Aisne (code: String = "02", name: String = "Aisne")
  case Allier (code: String = "03", name: String = "Allier")
  case AlpesdeHauteProvence (code: String = "04", name: String = "Alpes-de-Haute-Provence")
  case HautesAlpes (code: String = "05", name: String = "Hautes-Alpes")
  case AlpesMaritimes (code: String = "06", name: String = "Alpes-Maritimes")
  case Ardeche (code: String = "07", name: String = "Ardèche")
  case Ardennes (code: String = "08", name: String = "Ardennes")
  case Ariege (code: String = "09", name: String = "Ariège")
  case Aube (code: String = "10", name: String = "Aube")
  case Aude (code: String = "11", name: String = "Aude")
  case Aveyron (code: String = "12", name: String = "Aveyron")
  case BouchesduRhone (code: String = "13", name: String = "Bouches-du-Rhône")
  case Calvados (code: String = "14", name: String = "Calvados")
  case Cantal (code: String = "15", name: String = "Cantal")
  case Charente (code: String = "16", name: String = "Charente")
  case CharenteMaritime (code: String = "17", name: String = "Charente-Maritime")
  case Cher (code: String = "18", name: String = "Cher")
  case Correze (code: String = "19", name: String = "Corrèze")
  case Corse (code: String = "2A", name: String = "Corse-du-Sud")
  case Corse2 (code: String = "2B", name: String = "Haute-Corse")
  case CotedOr (code: String = "21", name: String = "Côte-d'Or")
  case CotesdArmor (code: String = "22", name: String = "Côtes-d'Armor")
  case Creuse (code: String = "23", name: String = "Creuse")
  case Dordogne (code: String = "24", name: String = "Dordogne")
  case Doubs (code: String = "25", name: String = "Doubs")
  case Drome (code: String = "26", name: String = "Drôme")
  case Eure (code: String = "27", name: String = "Eure")
  case EureetLoir (code: String = "28", name: String = "Eure-et-Loir")
  case Finistere (code: String = "29", name: String = "Finistère")
  case Gard (code: String = "30", name: String = "Gard")
  case HauteGaronne (code: String = "31", name: String = "Haute-Garonne")
  case Gers (code: String = "32", name: String = "Gers")
  case Gironde (code: String = "33", name: String = "Gironde")
  case Herault (code: String = "34", name: String = "Hérault")
  case IlleetVilaine (code: String = "35", name: String = "Ille-et-Vilaine")
  case Indre (code: String = "36", name: String = "Indre")
  case IndreetLoire (code: String = "37", name: String = "Indre-et-Loire")
  case Isere (code: String = "38", name: String = "Isère")
  case Jura (code: String = "39", name: String = "Jura")
  case Landes (code: String = "40", name: String = "Landes")
  case LoiretCher (code: String = "41", name: String = "Loir-et-Cher")
  case Loire (code: String = "42", name: String = "Loire")
  case HauteLoire (code: String = "43", name: String = "Haute-Loire")
  case LoireAtlantique (code: String = "44", name: String = "Loire-Atlantique")
  case Loiret (code: String = "45", name: String = "Loiret")
  case Lot (code: String = "46", name: String = "Lot")
  case LotetGaronne (code: String = "47", name: String = "Lot-et-Garonne")
  case Lozere (code: String = "48", name: String = "Lozère")
  case MaineetLoire (code: String = "49", name: String = "Maine-et-Loire")
  case Manche (code: String = "50", name: String = "Manche")
  case Marne (code: String = "51", name: String = "Marne")
  case HauteMarne (code: String = "52", name: String = "Haute-Marne")
  case Mayenne (code: String = "53", name: String = "Mayenne")
  case MeurtheetMoselle (code: String = "54", name: String = "Meurthe-et-Moselle")
  case Meuse (code: String = "55", name: String = "Meuse")
  case Morbihan (code: String = "56", name: String = "Morbihan")
  case Moselle (code: String = "57", name: String = "Moselle")
  case Nievre (code: String = "58", name: String = "Nièvre")
  case Nord (code: String = "59", name: String = "Nord")
  case Oise (code: String = "60", name: String = "Oise")
  case Orne (code: String = "61", name: String = "Orne")
  case PasdeCalais (code: String = "62", name: String = "Pas-de-Calais")
  case PuydeDome (code: String = "63", name: String = "Puy-de-Dôme")
  case PyreneesAtlantiques (code: String = "64", name: String = "Pyrénées-Atlantiques")
  case HautesPyrenees (code: String = "65", name: String = "Hautes-Pyrénées")
  case PyreneesOrientales (code: String = "66", name: String = "Pyrénées-Orientales")
  case BasRhin (code: String = "67", name: String = "Bas-Rhin")
  case HautRhin (code: String = "68", name: String = "Haut-Rhin")
  case Rhone (code: String = "69", name: String = "Rhône")
  case HauteSaone (code: String = "70", name: String = "Haute-Saône")
  case SaoneetLoire (code: String = "71", name: String = "Saône-et-Loire")
  case Sarthe (code: String = "72", name: String = "Sarthe")
  case Savoie (code: String = "73", name: String = "Savoie")
  case HauteSavoie (code: String = "74", name: String = "Haute-Savoie")
  case Paris (code: String = "75", name: String = "Paris")
  case SeineMaritime (code: String = "76", name: String = "Seine-Maritime")
  case SeineetMarne (code: String = "77", name: String = "Seine-et-Marne")
  case Yvelines (code: String = "78", name: String = "Yvelines")
  case DeuxSevres (code: String = "79", name: String = "Deux-Sèvres")
  case Somme (code: String = "80", name: String = "Somme")
  case Tarn (code: String = "81", name: String = "Tarn")
  case TarnetGaronne (code: String = "82", name: String = "Tarn-et-Garonne")
  case Var (code: String = "83", name: String = "Var")
  case Vaucluse (code: String = "84", name: String = "Vaucluse")
  case Vendee (code: String = "85", name: String = "Vendée")
  case Vienne (code: String = "86", name: String = "Vienne")
  case HauteVienne (code: String = "87", name: String = "Haute-Vienne")
  case Vosges (code: String = "88", name: String = "Vosges")
  case Yonne (code: String = "89", name: String = "Yonne")
  case TerritoiredeBelfort (code: String = "90", name: String = "Territoire de Belfort")
  case Essonne (code: String = "91", name: String = "Essonne")
  case HautsdeSeine (code: String = "92", name: String = "Hauts-de-Seine")
  case SeineSaintDenis (code: String = "93", name: String = "Seine-Saint-Denis")
  case ValdeMarne (code: String = "94", name: String = "Val-de-Marne")
  case ValdOise (code: String = "95", name: String = "Val-d'Oise")

enum Region :
  case AuvergneRhoneAlpes (code: String = "84", name: String = "Auvergne-Rhône-Alpes")
  case BourgogneFrancheComte (code: String = "27", name: String = "Bourgogne-Franche-Comté")
  case Bretagne (code: String = "53", name: String = "Bretagne")
  case CentreValdeLoire (code: String = "24", name: String = "Centre-Val de Loire")
  case Corse (code: String = "94", name: String = "Corse")
  case GrandEst (code: String = "44", name: String = "Grand Est")
  case HautsdeFrance (code: String = "32", name: String = "Hauts-de-France")
  case IledeFrance (code: String = "11", name: String = "Île-de-France")
  case Normandie (code: String = "28", name: String = "Normandie")
  case NouvelleAquitaine (code: String = "75", name: String = "Nouvelle-Aquitaine")
  case Occitanie (code: String = "76", name: String = "Occitanie")
  case PaysdelaLoire (code: String = "52", name: String = "Pays de la Loire")
  case ProvenceAlpesCotedAzur (code: String = "93", name: String = "Provence-Alpes-Côte d'Azur")

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
