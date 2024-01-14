import java.io.FileInputStream
import java.sql.{Connection, DriverManager, SQLException}
import scala.collection.mutable.ListBuffer

def createTableIfNotExists_GasStationsByRegDept(dbConnection: Connection): Unit = {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS GasStationsByRegDept (
    ID INT PRIMARY KEY,
    nb_gas_stations INT,
    name VARCHAR(255),
    type VARCHAR(3) CHECK (type IN ('DPT', 'REG'))
    );""".stripMargin)
}

def createTableIfNotExists_AvgGasPricesByRegDept(dbConnection: Connection): Unit = {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS AvgGasPricesByRegDept (
    ID INT PRIMARY KEY,
    department_or_region_name VARCHAR(255),
    gas_type VARCHAR(50),
    price DECIMAL(10, 2)
    );""".stripMargin)
}

def createTableIfNotExists_MostPresentGasStationServices(dbConnection: Connection): Unit = {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS AvgGasPricesByRegDept (
    ID INT PRIMARY KEY,
    gas_station_id INT,
    service_type VARCHAR(255)
    );""".stripMargin)
}

def createTableIfNotExists_DptMostGasStations(dbConnection: Connection): Unit = {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS DptMostGasStations (
    ID INT PRIMARY KEY,
    name VARCHAR(255),
    nb_gas_stations INT
    );""".stripMargin)
}

def createTableIfNotExists_MostExpensiveGasType(dbConnection: Connection): Unit = {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS MostExpensiveGasType (
    ID INT PRIMARY KEY,
    name VARCHAR(255),
    nb_gas_stations INT
    );""".stripMargin)
}
