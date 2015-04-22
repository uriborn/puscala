name := "puscala"

version := "1.0.0"

scalaVersion := "2.11.6"

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-unchecked")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor"      % "2.3.9",
  "ch.qos.logback"    %  "logback-classic" % "1.1.2",
  "net.liftweb"       %% "lift-json"       % "2.6",
  "org.specs2"        %% "specs2-core"     % "3.5" % "test"
)
