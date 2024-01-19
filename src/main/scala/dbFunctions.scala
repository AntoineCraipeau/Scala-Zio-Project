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
      ID INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
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

def insertIntoGasStationsByRegDept(dbConnection: Connection, nbGasStations: Int, code: Int, stationType: String): ZIO[Any, Throwable, Unit] = {
  val insertStatement =
    s"""INSERT INTO PUBLIC.GasStationsByRegDept (nb_gas_stations, code, type)
        VALUES ($nbGasStations, $code, '$stationType');"""

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.executeUpdate(insertStatement)
        println(s"Inserted gas station record: ID=nb_gas_stations=$nbGasStations, code=$code, type=$stationType")
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
  val createTableStatement =
  """CREATE TABLE IF NOT EXISTS AvgGasPricesByRegDept (
  ID INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
  gas_type VARCHAR(50),
  price DOUBLE,
  code INT,
  type VARCHAR(3) CHECK (type IN ('DPT', 'REG'))
  );""".stripMargin

ZIO.scoped {
  ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
    ZIO.attemptBlocking {
      stmt.execute(createTableStatement)
      println("Table AvgGasPricesByRegDept created successfully")
      }
    }
  }
}


def insertIntoAvgPricesByRegDept(dbConnection: Connection, price: Double, code: Int, stationType: String, gasType: String): ZIO[Any, Throwable, Unit] = {
  val insertStatement =
    s"""INSERT INTO PUBLIC.AvgGasPricesByRegDept (gas_type, price, code, type)
        VALUES ('$gasType', $price, $code, '$stationType');"""

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.executeUpdate(insertStatement)
        println(s"Inserted gas price record: ID=nb_gas_stations= gasType=$gasType , price=$price , code=$code , type=$stationType")
      }
    }
  }
}


def selectAvgPricesByCode(dbConnection: Connection, code: Int, zoneType: String, gasType: String): ZIO[Any, Throwable, Option[Double]] = {
  ZIO.attempt {
    val stmt = dbConnection.createStatement()

    val sql = "SELECT * FROM AvgGasPricesByRegDept WHERE code = ? AND type = ? AND gas_type = ?"
    val preparedStatement = dbConnection.prepareStatement(sql)
    preparedStatement.setInt(1, code)
    preparedStatement.setString(2, zoneType)
    preparedStatement.setString(3, gasType)

    val resultSet = preparedStatement.executeQuery()

    if (resultSet.next()) {
      Some(resultSet.getInt("price")).map(_.toDouble)
    } else {
      None
    }
  }
}

def createTableIfNotExists_MostPresentGasStationServices(dbConnection: Connection): ZIO[Any, Any, Unit] = {
  val createTableStatement =
    """CREATE TABLE IF NOT EXISTS MostPresentGasStationServices (
  ID INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
  count INT,
  service_type VARCHAR(255)
  );""".stripMargin


  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.execute(createTableStatement)
        println("Table MostPresentGasStationServices created successfully")
      }
    }
  }
}


def selectMostPresentServices(dbConnection: Connection): ZIO[Any, Throwable, List[(String, Int)]] = {
  val selectStatement = "SELECT service_type, count FROM MostPresentGasStationServices"

  ZIO.attemptBlocking {
    val resultSet = dbConnection.createStatement().executeQuery(selectStatement)
    val buffer = scala.collection.mutable.ListBuffer.empty[(String, Int)]
    while (resultSet.next()) {
      val service = resultSet.getString("service_type")
      val count = resultSet.getInt("count")
      buffer += (service -> count)
    }
    buffer.toList
  }
}


def insertMostPresentServices(dbConnection: Connection, services: Seq[(ExtraServices, Int)]): ZIO[Any, Throwable, Unit] = {
  val insertStatement = "INSERT INTO MostPresentGasStationServices (service_type, count) VALUES (?, ?)"

  ZIO.attemptBlocking {
    val preparedStatement = dbConnection.prepareStatement(insertStatement)
    services.foreach { case (service, count) =>
      preparedStatement.setString(1, service.toString)
      preparedStatement.setInt(2, count)
      preparedStatement.addBatch()
    }
    preparedStatement.executeBatch()
    preparedStatement.close()
  }
}





def selectAvgGasPricesByRegDept(dbConnection: Connection, code: Int, zoneType: String): ZIO[Any, Throwable, Option[Double]] = {
  ZIO.attempt {
    val stmt = dbConnection.createStatement()

    val sql = "SELECT * FROM AvgGasPricesByRegDept WHERE code = ? AND type = ?"
    val preparedStatement = dbConnection.prepareStatement(sql)
    preparedStatement.setInt(1, code)
    preparedStatement.setString(2, zoneType)

    val resultSet = preparedStatement.executeQuery()

    if (resultSet.next()) {
      Some(resultSet.getInt("price")).map(_.toDouble)
    } else {
      None
    }
  }
}

def createTableIfNotExists_DptMostGasStations(dbConnection: Connection): ZIO[Any, Any, Unit] = {
  val createTableStatement =
    """CREATE TABLE IF NOT EXISTS DptMostGasStations (
    name VARCHAR(255),
    nb_gas_stations INT
    );""".stripMargin

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.execute(createTableStatement)
        println("Table MostPresentGasStationServices created successfully")
      }
    }
  }
}

def selectDptMostGasStations(dbConnection: Connection): ZIO[Any, Throwable, Option[(String, Int)]] = {
  ZIO.attempt {
    val stmt = dbConnection.createStatement()

    val sql = "SELECT * FROM DptMostGasStations"
    val preparedStatement = dbConnection.prepareStatement(sql)

    val resultSet = preparedStatement.executeQuery()

    if (resultSet.next()) {
      Some((resultSet.getString("name"),resultSet.getInt("nb_gas_stations")))
    } else {
      None
    }
  }
}

def insertIntoDptMostGasStations(dbConnection: Connection, nbGasStations: Int, name: String): ZIO[Any, Throwable, Unit] = {
  val insertStatement =
    s"""INSERT INTO PUBLIC.DptMostGasStations (name, nb_gas_stations)
        VALUES ('$name', $nbGasStations);"""

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.executeUpdate(insertStatement)
        println(s"Inserted gas station record: ID=nb_gas_stations=$nbGasStations, name=$name")
      }
    }
  }
}

def createTableIfNotExists_MostExpensiveGasType(dbConnection: Connection): ZIO[Any, Any, Unit] = {
  val createTableStatement =
    """CREATE TABLE IF NOT EXISTS MostExpensiveGasType (
    gas_type VARCHAR(255),
    average_price DOUBLE
    );""".stripMargin

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.execute(createTableStatement)
        println("Table MostPresentGasStationServices created successfully")
      }
    }
  }
}

def selectMostExpensiveGas(dbConnection: Connection): ZIO[Any, Throwable, Option[Double]] = {
  ZIO.attempt {
    val stmt = dbConnection.createStatement()

    val sql = "SELECT * FROM MostExpensiveGasType"
    val preparedStatement = dbConnection.prepareStatement(sql)

    val resultSet = preparedStatement.executeQuery()

    if (resultSet.next()) {
      Some(resultSet.getInt("average_price")).map(_.toDouble)
    } else {
      None
    }
  }
}

def insertMostExpensiveGas(dbConnection: Connection, gasType: String, averagePrice: Double): ZIO[Any, Throwable, Unit] = {
  val insertStatement =
    s"""INSERT INTO PUBLIC.MostExpensiveGasType (gas_type, average_price)
        VALUES ('$gasType', $averagePrice);"""

  ZIO.scoped {
    ZIO.acquireRelease(ZIO.attempt(dbConnection.createStatement()))(stmt => ZIO.attempt(stmt.close()).orDie).flatMap { stmt =>
      ZIO.attemptBlocking {
        stmt.executeUpdate(insertStatement)
        println(s"Inserted gas station record: gasType=$gasType, averagePrice=$averagePrice")
      }
    }
  }
}