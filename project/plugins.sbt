// Comment to get more information during initialization
logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

// OS> activator dependencyUpdates
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.6")

// OS> activator scalastyle
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.6.0")

// IntelliJ IDEA plugin
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.7.0-SNAPSHOT")

