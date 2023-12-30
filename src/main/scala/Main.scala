import zio.*
import zio.Console.*
import com.github.tototoshi.csv.*
import zio.stream.ZSink

object Main extends ZIOAppDefault {
  override def run: ZIO[Any & (ZIOAppArgs & Console), Any, Unit] =
    for {
      _ <- printLine("Welcome to Gas Station Treatments!")
      _ <- ConsoleApp.runMenu()
    } yield ()
}