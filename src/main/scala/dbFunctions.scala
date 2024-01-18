import java.io.FileInputStream
import java.sql.{Connection, DriverManager, SQLException}
import scala.collection.mutable.ListBuffer
import zio.*
import zio._
import java.sql.Connection
import java.sql.Statement

import zio._
import java.sql.Connection

def createTableIfNotExists_GasStationsByRegDept(dbConnection: Connection): ZIO[Any, Throwable, Unit] = {
  val createTableStatement =
    """CREATE TABLE IF NOT EXISTS PUBLIC.GasStationsByRegDept (
      ID INT PRIMARY KEY,
      nb_gas_stations INT,
      code INT,
      type VARCHAR(3) CHECK (type IN ('DPT', 'REG'))
      );""".stripMargin

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.execute(createTableStatement)
        println("Table GasStationsByRegDept created successfully")
      }
    }
  }
}

def insertIntoGasStationsByRegDept(
                                    dbConnection: Connection,
                                    id: Int,
                                    nbGasStations: Int,
                                    code: Int,
                                    stationType: String
                                  ): ZIO[Any, Throwable, Unit] = {
  val insertStatement =
    s"""INSERT INTO PUBLIC.GasStationsByRegDept (ID, nb_gas_stations, code, type)
        VALUES ($id, $nbGasStations, $code, '$stationType');"""

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.executeUpdate(insertStatement)
        println(s"Inserted gas station record: ID=$id, nb_gas_stations=$nbGasStations, code=$code, type=$stationType")
      }
    }
  }
}

def selectStationsByCode(dbConnection: Connection, code: Int, zoneType: String): ZIO[Any, Throwable, Option[Int]] = {
  ZIO.attempt {
  val stmt = dbConnection.createStatement()

  val sql = "SELECT * FROM GasStationsByRegDept WHERE code = ? AND type = ?"
  val preparedStatement = dbConnection.prepareStatement(sql)
  preparedStatement.setInt(1, code)
  preparedStatement.setString(2, zoneType)

  val resultSet = preparedStatement.executeQuery()


    if (resultSet.next()) {
      Some(resultSet.getInt("nb_gas_stations"))
    } else {
      None
    }
  }
}

def createTableIfNotExists_AvgGasPricesByRegDept(dbConnection: Connection): ZIO[Any, Any, Unit] = {
  ZIO.attempt {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS AvgGasPricesByRegDept (
    ID INT PRIMARY KEY,
    department_or_region_name VARCHAR(255),
    gas_type VARCHAR(50),
    price DECIMAL(10, 2)
    );""".stripMargin)
  }.unit
}

def createTableIfNotExists_MostPresentGasStationServices(dbConnection: Connection): ZIO[Any, Any, Unit] = {
  ZIO.attempt {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS AvgGasPricesByRegDept (
    ID INT PRIMARY KEY,
    gas_station_id INT,
    service_type VARCHAR(255)
    );""".stripMargin)
  }.unit
}

def createTableIfNotExists_DptMostGasStations(dbConnection: Connection): ZIO[Any, Any, Unit] = {
  ZIO.attempt {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS DptMostGasStations (
    ID INT PRIMARY KEY,
    name VARCHAR(255),
    nb_gas_stations INT
    );""".stripMargin)
  }.unit
}

def createTableIfNotExists_MostExpensiveGasType(dbConnection: Connection): ZIO[Any, Any, Unit] = {
  ZIO.attempt {
  val stmt = dbConnection.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS MostExpensiveGasType (
    ID INT PRIMARY KEY,
    name VARCHAR(255),
    nb_gas_stations INT
    );""".stripMargin)
  }
}
