val dottyVersion = "0.18.1-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "compose",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    scalacOptions ++= Seq(
      "-feature",
      "-language:higherKinds,existentials", 
    ),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
