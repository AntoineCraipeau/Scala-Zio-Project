import zio.ZIO
import zio.test._
import zio.test.Assertion.*

def returnString(str: String): ZIO[Any, Nothing, String] =
  ZIO.succeed(str)


object ExampleSpec extends ZIOSpecDefault {
  override def spec = suite("TestingApplicationsExamplesSpec")(


    test("returnString correctly returns string") {
      val testString = "Hello World!"
      for {
        output <- returnString(testString)
      } yield assertTrue(output == testString)


    }
  )
}