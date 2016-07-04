resolvers += Resolver.file("local ivy", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "3.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")

addSbtPlugin("com.github.fedragon" % "sbt-todolist" % "0.6")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")