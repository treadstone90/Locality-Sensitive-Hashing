name := "Locality Sensitive Hashing"

scalaVersion := "2.11.8"

retrieveManaged := true

crossPaths := false

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.1.2",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16"
)

