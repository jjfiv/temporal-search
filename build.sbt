name := "Temporal Search"

version := "0.2"

organization := "com.github.jjfiv"

scalaVersion := "2.10.1"

resolvers += "Local Maven" at Path.userHome.asFile.toURI.toURL + ".m2/repository"



{
  val galagoVersion = "3.4"
  libraryDependencies += "org.lemurproject.galago" % "core" % galagoVersion
  libraryDependencies += "org.lemurproject.galago" % "contrib" % galagoVersion
  //libraryDependencies += "org.lemurproject.galago" % "tupleflow" % galagoVersion
}

libraryDependencies += "junit" % "junit" % "4.10"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"
