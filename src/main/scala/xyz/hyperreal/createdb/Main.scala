package xyz.hyperreal.createdb

import xyz.hyperreal.char_reader.CharReader
import scopt.OParser
import xyz.hyperreal.datetime.Datetime
import xyz.hyperreal.importer.Importer.importFromReader
import xyz.hyperreal.importer.{Column, Enum, Import, Table}

import java.io.File

object Main extends App {

  case class Config(input: File)

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("createdb"),
      head("createdb", "0.1.0"),
      help("help").text("prints this usage text"),
      arg[File]("<file>")
        .action((f, c) => c.copy(input = f))
        .text("input file")
    )
  }

  OParser.parse(parser, args, Config(null)) match {
    case Some(config) => generate(config.input.getAbsolutePath)
    case _ =>
  }

  def generate(tab: String): Unit = {
    val Import(enums, tables) = importFromReader(CharReader.fromFile(tab), doubleSpaces = true)
    val enumset = enums map (_.name) toSet

    for (Enum(name, labels) <- enums)
      println(s"""CREATE TYPE "$name" AS ENUM (${labels map (l => s"'$l'") mkString ", "});""")

    for (Table(name, header, _) <- tables) {
      val columns =
        for (Column(name, typ, args) <- header)
          yield {
            val ctyp =
              typ match {
                case "integer" => "INTEGER"
                case "bigint" => "BIGINT"
                case "text" => "TEXT"
                case "float" => "DOUBLE PRECISION"
                case "decimal" => "NUMERIC(15, 2)"
                case "uuid" => "UUID"
                case "timestamp" => "TIMESTAMP WITHOUT TIME ZONE"
                case _ if enumset contains typ => typ
              }

            s"""  "$name" $ctyp${if (args == List("pk")) " PRIMARY KEY" else ""}"""
          }

      println(
        s"""
           |CREATE TABLE "$name" (
           |${columns mkString ",\n"}
           |);""".trim.stripMargin
      )
    }

    for (Table(name, header, _) <- tables)
      for (Column(col, _, args) <- header)
        args match {
          case List("fk", ft, _) => println(s"""ALTER TABLE "$name" ADD FOREIGN KEY ("$col") REFERENCES "$ft";""")
          case _ =>
        }

    for (Table(name, header, data) <- tables if data.nonEmpty) {
      val rows =
        data map (r =>
          r map {
            case d: Datetime => s"'$d'"
            case s: String => s"'${s.replace("'", "''")}'"
            case null => "NULL"
            case v         => v
          } mkString ("  (", ", ", ")")) mkString ",\n"

      println(s"INSERT INTO \"$name\" (${header map { case Column(name, _, _) => s"\"$name\"" } mkString ", "}) VALUES\n$rows;")
    }
  }

}
