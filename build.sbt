ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "antonkw"

lazy val cnf = (project in file(".")).settings(name := "2CNF-solver")

mainClass in(Compile, run) := Some("CnfSolver")