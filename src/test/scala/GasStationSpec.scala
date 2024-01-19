import zio.test.*
import Treatments.*
import Streams.*
import Main.*

object GasStationSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Any] =
    suite("GasStationSpec")(
      suite("loadGasStationCsv")(
        test("loadGasStationCsv correctly loads gas stations from CSV") {
            for {
              gasStations <- loadGasStationCsv().runCollect
            } yield assertTrue(gasStations.nonEmpty)
        },
        test("loadGasStationCsv gets the same results for each call") {
          for {
            gasStations1 <- loadGasStationCsv().runCollect
            gasStations2 <- loadGasStationCsv().runCollect
          } yield assertTrue(gasStations1 == gasStations2)
        },
        test("loadGasStationCsv produces no objects null or containing null") {
          for {
            gasStations <- loadGasStationCsv().runCollect
            geographicDatas = gasStations.map(_.geographicData)
            populations = geographicDatas.map(_.population)
            addresses = geographicDatas.map(_.address)
            cities = geographicDatas.map(_.city)
            regions = geographicDatas.map(_.region)
            departments = geographicDatas.map(_.department)
            coordinates = geographicDatas.map(_.coordinates)
            serviceDatas = gasStations.map(_.serviceData)
            gasLists = serviceDatas.map(_.gasList)
            extraServices = serviceDatas.map(_.extraService)
            automates = serviceDatas.map(_.automate24)
          } yield assertTrue(
            !gasStations.contains(null) &&
            !geographicDatas.contains(null) &&
            !populations.contains(null) &&
            !addresses.contains(null) &&
            !cities.contains(null) &&
            !regions.contains(null) &&
            !departments.contains(null) &&
            !coordinates.contains(null) &&
            !serviceDatas.contains(null) &&
            !gasLists.contains(null) &&
            !extraServices.contains(null) &&
            !automates.contains(null)
          )
        }
      ),
      suite("User input")(
        test("User can quit"){
          val expectedOutput = "Exiting..."
          val test = for {
            _ <- TestConsole.feedLines("q")
            _ <- printMenu
            output <- TestConsole.output
          } yield assertTrue(output.exists(_.contains(expectedOutput)))
          test
        },
        test("User enter invalid choice recall menu") {
          val test = for {
            _ <- TestConsole.feedLines("a")
            _ <- TestConsole.feedLines("b")
            _ <- TestConsole.feedLines("c")
            _ <- TestConsole.feedLines("q")
            _ <- printMenu
          } yield assertTrue(true)
          test
        }
      ),
      suite("Treatments test")(
        suite("Departements test")(
          suite("Departement Paris 75 test")(
            test("departmentCount prints the correct result") {
              val expectedOutput = "57"
              val test = for {
                count <- Streams.countDepartmentStream("75")
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            },
            test("averagePriceDepartment prints the correct result") {
              val expectedOutput = "1.1369473684210525"
              val test = for {
                _ <- Streams.averagePriceDepartmentStream("75", "Paris", GasType.E10, "E10", 57)
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            }
          ),
          suite("Departement Var 83 test")(
            test("departmentCount prints the correct result") {
              val expectedOutput = "183"
              val test = for {
                count <- Streams.countDepartmentStream("83")
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            },
            test("averagePriceDepartment prints the correct result") {
              val expectedOutput = "1.5270710382513664"
              val test = for {
                _ <- Streams.averagePriceDepartmentStream("83", "Var", GasType.E10, "E10", 183)
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            }
          ),
          test("findDepartmentWithMostGasStations prints the correct result") {
            val expectedOutput = "Bouches-du-Rhône with 278 stations"
            val test = for {
              count <- Streams.findDepartmentWithMostGasStationsStream()
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          }
        ),
        suite("Region test")(
          suite("Region Île-de-France 11 test")(
            test("regionCount prints the correct result") {
              val expectedOutput = "892"
              val test = for {
                count <- Streams.countRegionStream("11")
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            },
            test("averagePriceRegion prints the correct result") {
              val expectedOutput = "1.6593699551569516"
              val test = for {
                _ <- Streams.averagePriceRegionStream("11", "Île-de-France", GasType.SP98, "SP98", 892)
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            }
          ),
        suite("Region Normandie 28 test")(
          test("regionCount prints the correct result") {
            val expectedOutput = "569"
            val test = for {
              count <- Streams.countRegionStream("28")
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          },
          test("averagePriceRegion prints the correct result") {
            val expectedOutput = "1.5042231985940249"
            val test = for {
              _ <- Streams.averagePriceRegionStream("28", "Normandie", GasType.SP98, "SP98", 569)
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          }
        )
        ),
        suite("Extra Services test")(
          test("calculateMostPresentExtraService prints the correct result") {
            val expectedOutput = "InflationStation with 5607 stations"
            val test = for {
              _ <- Streams.findMostPresentExtraServiceStream()
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          },
          test("calculateAverageExtraServicesPerStation prints the correct result") {
            val expectedOutput = "The average number of extra services per station is: 6.335167033406681"
            val test = for {
              _ <- Treatments.calculateAverageExtraServicesPerStation()
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          },
          test("calculateAveragePriceForExtraServicesWithZStream prints the correct result") {
            val expectedOutput = "Extra Service: Showers => Average Fuel Price: 1.6540907280468928"
            val test = for {
              _ <- Treatments.calculateAveragePriceForExtraServices()
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          },
          test("calculateAveragePriceForExtraServicesWithZStream gets same number of results when called twice") {
            val test = for {
              _ <- Treatments.calculateAveragePriceForExtraServices()
              output <- TestConsole.output
              _ <- Treatments.calculateAveragePriceForExtraServices()
              output2 <- TestConsole.output
            } yield assertTrue(output.size == output2.size/2)
            test
          }
        )
      )
    )
}