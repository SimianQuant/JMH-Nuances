lazy val jmhnuances = project
  .in(file("."))
  .settings(
    name := "jmh-nuances",
    organization := "com.simianquant",
    version := Settings.versions.project,
    scalaVersion := Settings.versions.scala,
    scalacOptions ++= List(
      ("-Xlint:adapted-args,nullary-unit,inaccessible,nullary-override,infer-any,doc-detached,private-shadow," +
        "type-parameter-shadow,poly-implicit-overload,option-implicit,delayedinit-select,by-name-right-associative," +
        "package-object-classes,unsound-match,stars-align,constant"),
      "-Ywarn-unused:imports,patvars,privates,locals",
      "-opt:l:method",
      "-Ywarn-unused-import",
      "-deprecation",
      "-unchecked",
      "-explaintypes",
      "-encoding",
      "UTF-8",
      "-feature",
      "-Xlog-reflective-calls",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Xfuture"
    ),
    fork := true,
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % Settings.versions.commonsMath,
      "org.typelevel" %% "spire" % Settings.versions.spire,
      "com.typesafe.play" %% "play-json" % Settings.versions.play
    )
  )
  .enablePlugins(JmhPlugin)
