import zio.ZIO
import zio.test._
import zio.test.Assertion.*

object GasStationSpec extends ZIOSpecDefault {
  override def spec = suite("GasStationSpec")(
      test("loadGasStationCsv correctly loads gas stations from CSV") {
        for {
          gasStations <- loadGasStationCsv().runCollect
        } yield assert(gasStations.nonEmpty)(isTrue)

    }
  )
}