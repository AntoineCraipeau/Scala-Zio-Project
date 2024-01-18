import zio.ZIO
import zio.test._
import zio.test.Assertion.*
import Treatments.*

object GasStationSpec extends ZIOSpecDefault {
  override def spec =
    suite("GasStationSpec")(
      suite("loadGasStationCsv")(
        test("loadGasStationCsv correctly loads gas stations from CSV") {
            for {
              gasStations <- loadGasStationCsv().runCollect
            } yield assert(gasStations.nonEmpty)(isTrue)
        }
      ),
      suite("Paris test")(
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
    )
    )
}