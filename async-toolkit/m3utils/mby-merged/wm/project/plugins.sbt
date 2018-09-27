addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.1")
addSbtPlugin("org.ensime" % "sbt-ensime" % "2.6.1")
// dirty-money contains 'cleanCache' task
// you can execute e.g. 'cleanCache "com.intel.cg.hpfd" % "csr-model"' to
// wipeout csr-model from your local ivy cache
addSbtPlugin("com.eed3si9n" % "sbt-dirty-money" % "0.2.0")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-license-report" % "1.2.0")
