case class GasStation(
                       id: Int,
                       population: Population,
                       address: String,
                       city: String,
                       schedule: Option[String],
                       service: List[Services],
                       automate24: Boolean,
                       region: Location,
                       department: Location,
                       gasList: List[Gas],
                       latitude : Long,
                       longitude : Long
                     )

case class Location(
                     code: String,
                     name: String
                   )

enum GasType:
  case SP98, SP95, Gazol, E10, Diesel, E85, GPLc

case class Gas(
                gasType: GasType,
                available: Boolean,
                price: Double
              )

enum Services:
  case PublicToilets, Laundry, ParcelRelay, FoodShop, TakeAwayFood, SitInRestaurant, Bar, LampOilSales, InflationStation, AdditiveFuel, VehicleRental, HeavyVehicleLane, AutomaticCarWash, ManualCarWash, DomesticGasSales, Wifi, ATM24_7CashMachine, CashDispenser

enum Population:
  case Route, Autoroute
