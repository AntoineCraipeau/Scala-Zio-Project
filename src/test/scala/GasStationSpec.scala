import zio.ZIO
import zio.test.*
import zio.test.Assertion.*
import Treatments.*
import Main.*
import zio.json.ast.Json.Bool.True


object GasStationSpec extends ZIOSpecDefault {
  override def spec =
    suite("GasStationSpec")(
      suite("loadGasStationCsv")(
        test("loadGasStationCsv correctly loads gas stations from CSV") {
            for {
              gasStations <- loadGasStationCsv().runCollect
            } yield assertTrue(gasStations.nonEmpty)
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
                _ <- TestConsole.feedLines("75")
                count <- Treatments.departmentCount("1")
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            },
            test("averagePriceDepartment prints the correct result") {
              val expectedOutput = "1.1369473684210525"
              val test = for {
                _ <- Treatments.averagePriceDepartment("75", "Paris", GasType.E10, "E10", 57)
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            }
          ),
          suite("Departement Var 83 test")(
            test("departmentCount prints the correct result") {
              val expectedOutput = "183"
              val test = for {
                _ <- TestConsole.feedLines("83")
                count <- Treatments.departmentCount("1")
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            },
            test("averagePriceDepartment prints the correct result") {
              val expectedOutput = "1.5270710382513664"
              val test = for {
                _ <- Treatments.averagePriceDepartment("83", "Var", GasType.E10, "E10", 183)
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            }
          ),
          test("findDepartmentWithMostGasStations prints the correct result") {
            val expectedOutput = "BouchesduRhone with 278 stations"
            val test = for {
              count <- Treatments.findDepartmentWithMostGasStations()
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
                _ <- TestConsole.feedLines("11")
                count <- Treatments.regionCount("1")
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            },
            test("averagePriceRegion prints the correct result") {
              val expectedOutput = "1.6593699551569516"
              val test = for {
                _ <- Treatments.averagePriceRegion("11", "Île-de-France", GasType.SP98, "SP98", 892)
                output <- TestConsole.output
              } yield assertTrue(output.exists(_.contains(expectedOutput)))
              test
            }
          ),
        suite("Region Normandie 28 test")(
          test("regionCount prints the correct result") {
            val expectedOutput = "569"
            val test = for {
              _ <- TestConsole.feedLines("28")
              count <- Treatments.regionCount("1")
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          },
          test("averagePriceRegion prints the correct result") {
            val expectedOutput = "1.5042231985940249"
            val test = for {
              _ <- Treatments.averagePriceRegion("28", "Normandie", GasType.SP98, "SP98", 569)
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          }
        )
        ),
        suite("Extra Services test")(
          test("calculateMostPresentExtraService prints the correct result") {
            val expectedOutput = "Extra Service: InflationStation, Count: 5607"
            val test = for {
              _ <- Treatments.calculateMostPresentExtraService()
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
            val expectedOutput = "Extra Service: Showers, Average Price: 1.6540907280468928"
            val test = for {
              _ <- Treatments.calculateAveragePriceForExtraServicesWithZStream()
              output <- TestConsole.output
            } yield assertTrue(output.exists(_.contains(expectedOutput)))
            test
          }
        )
      )
    )
}