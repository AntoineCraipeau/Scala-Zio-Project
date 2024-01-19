import zio.ZIO
import zio.test._
import zio.test.Assertion.*
import Treatments.*
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
        test("User can't enter invalid choice") {
          val expectedOutput = "Invalid choice. Please enter a valid option."
          val test = for {
            _ <- TestConsole.feedLines("a")
            _ <- printMenu
            output <- TestConsole.output
          } yield assertTrue(output.exists(_.contains(expectedOutput)))
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