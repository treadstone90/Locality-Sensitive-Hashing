name := "Locality Sensitive Hashing"

scalaVersion := "2.10.1"

retrieveManaged := true

crossPaths := false

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "0.8.1",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16"
)

