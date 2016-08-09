
lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature"),
  scalaVersion := "2.11.7",
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4",
      "junit" % "junit" % "4.10" % "test"))


def nio2check(): String = {
  val cls = "java.nio.channels.AsynchronousFileChannel"
  try {Class.forName(cls); ""}
  catch {case _: ClassNotFoundException =>
    ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
      "You are probably running Java < 1.7; answers will not compile.\n" +
      "You seem to be running " + System.getProperty("java.version") + ".\n" +
      "Try `project exercises' before compile, or upgrading your JDK.")
  }
}
lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "fpinscala",
    onLoadMessage ~= (_ + nio2check())
  ).
  aggregate(chapterCode, exercises, answers)


lazy val chapterCode = (project in file("chaptercode")).
  settings(commonSettings: _*).
  settings( name := "chapter-code")

lazy val exercises = (project in file("exercises")).
  settings(commonSettings: _*).
  settings( name := "exercises")

lazy val answers = (project in file("answers")).
  settings(commonSettings: _*).
  settings( name := "answers")

