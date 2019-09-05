val dottyVersion = "0.19.1-bin-20190904-beba63a-NIGHTLY"

val hedgehogVersion = "49859b13f023a70937c6e4ee9770fb84f63bdcc5"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tinyfp",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    resolvers += Resolver.url("bintray-scala-hedgehog",
        url("https://dl.bintray.com/hedgehogqa/scala-hedgehog")
      )(Resolver.ivyStylePatterns),

    scalacOptions ++= Seq(
      "-feature",
      "-language:higherKinds,existentials", 
    ),

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
    ),

    libraryDependencies ++= Seq(
      "hedgehog" %% "hedgehog-core" % hedgehogVersion,
      "hedgehog" %% "hedgehog-runner" % hedgehogVersion,
      "hedgehog" %% "hedgehog-sbt" % hedgehogVersion
    ).map(_.withDottyCompat(scalaVersion.value)),

    testFrameworks += TestFramework("hedgehog.sbt.Framework")
  )
