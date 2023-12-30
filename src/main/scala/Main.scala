import zio.*
import com.github.tototoshi.csv.*

implicit object CustomFormat extends DefaultCSVFormat { // Notre fichier CSV utilise le délimiteur ';' au lieu de ','
  override val delimiter = ';'
}

object App extends ZIOAppDefault {

  override def run: ZIO[Any & (ZIOAppArgs & Scope), Any, Unit] =
    for {
      stream <- makeTreatments()
      stream <- calculateMostExpensiveGas()
      stream <- calculateMostPresentExtraService()
      stream <- findDepartmentWithMostGasStations()
      stream <- calculateAverageExtraServicesPerStation()
    } yield ()

}