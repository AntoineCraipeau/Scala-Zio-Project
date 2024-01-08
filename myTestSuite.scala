import org.scalameta.munit.*

class MyTestSuite extends FunSuite {
  test("loadGasStationCsv should not return null") {
    val result = loadGasStationCsv().runCollect.map(_.nonNull)
    assert(result, "Gas stations loaded should not contain null values")
  }
}
