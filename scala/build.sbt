

val rxVersion = "0.4.0"
val scalatagsVersion = "0.6.5"
val scalaJSdomVersion = "0.9.2"
val scaladgetVersion = "1.2.3"
name := "zombies"

lazy val ode = Project("ode", file("ode")) enablePlugins(SbtOsgi) settings (
  scalaVersion := "2.12.8",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.5.0",
  OsgiKeys.exportPackage := Seq("zombies.ode.*;-split-package:=merge-first"),
  OsgiKeys.importPackage := Seq("*;resolution:=optional"),
  OsgiKeys.privatePackage := Seq("!scala.*,!java.*,!META-INF.*.RSA,!META-INF.*.SF,!META-INF.*.DSA,META-INF.services.*,META-INF.*,*"),
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))"""",
  mainClass in (Compile,run) := Some("zombies.ode.ODE")
)


lazy val model = Project("model", file("model")) enablePlugins(ScalaJSPlugin) settings(
  scalaVersion := "2.12.8",
)

lazy val bundle = Project("zombies-bundle", file("bundle")) enablePlugins(SbtOsgi) settings(
  scalaVersion := "2.12.8",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "org.locationtech.jts" % "jts-core" % "1.16.1",
  OsgiKeys.exportPackage := Seq("zombies.*;-split-package:=merge-first"),
  OsgiKeys.importPackage := Seq("*;resolution:=optional"),
  OsgiKeys.privatePackage := Seq("!scala.*,!java.*,!monocle.*,!META-INF.*.RSA,!META-INF.*.SF,!META-INF.*.DSA,META-INF.services.*,META-INF.*,*"),
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))"""",
  osgiSettings
) dependsOn(model,ode)


lazy val console = Project("console", file("console")) dependsOn (bundle) settings (
  libraryDependencies += "com.github.tomas-langer" % "chalk" % "1.0.2")


lazy val buildGUI = taskKey[Unit]("buildGUI")

lazy val guiDependencies = Seq(
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % scalaJSdomVersion,
  libraryDependencies += "com.lihaoyi" %%% "scalarx" % rxVersion,
  libraryDependencies += "fr.iscpif.scaladget" %%% "svg" % scaladgetVersion,
  libraryDependencies += "fr.iscpif.scaladget" %%% "bootstrapnative" % scaladgetVersion,
  libraryDependencies += "fr.iscpif.scaladget" %%% "bootstrapslider" % scaladgetVersion,
  libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.2",
)

def guiBuilder(demoTarget: File, demoResource: File, jsBuild: File, dependencyJS: File, depsCSS: File, globalCSS: File) = {
  IO.copyFile(jsBuild, demoTarget / "js/demo.js")
  IO.copyFile(dependencyJS, demoTarget / "js/deps.js")
  IO.copyDirectory(globalCSS, demoTarget / "css")
  IO.copyDirectory(depsCSS, demoTarget / "css")
  IO.copyDirectory(demoResource, demoTarget)
}

lazy val guiUtils = Project("guiUtils", file("guiUtils")) dependsOn (model) enablePlugins (ExecNpmPlugin) settings (
  scalaVersion := "2.12.8",
  guiDependencies)

lazy val zombieland = Project("zombieland", file("zombieland")) dependsOn (guiUtils) enablePlugins (ExecNpmPlugin) settings (
  scalaVersion := "2.12.8",
  buildGUI := guiBuilder(target.value, (resourceDirectory in Compile).value, (fullOptJS in Compile).value.data, dependencyFile.value, cssFile.value, (resourceDirectory in guiUtils in Compile).value / "css"))

lazy val cooperation = Project("cooperation", file("cooperation")) dependsOn (guiUtils) enablePlugins(ExecNpmPlugin) settings(
  scalaVersion := "2.12.8",
  buildGUI := guiBuilder(target.value, (resourceDirectory in Compile).value, (fullOptJS in Compile).value.data, dependencyFile.value, cssFile.value, (resourceDirectory in guiUtils in Compile).value / "css")
)

lazy val cooperationandarmy = Project("cooperationandarmy", file("cooperationandarmy")) dependsOn (guiUtils) enablePlugins(ExecNpmPlugin) settings(
  scalaVersion := "2.12.8",
  buildGUI := guiBuilder(target.value, (resourceDirectory in Compile).value, (fullOptJS in Compile).value.data, dependencyFile.value, cssFile.value, (resourceDirectory in guiUtils in Compile).value / "css")
)

lazy val spatialsens = Project("spatialsens", file("spatialsens")) dependsOn (guiUtils) enablePlugins(ExecNpmPlugin) settings(
  scalaVersion := "2.12.8",
  buildGUI := guiBuilder(target.value, (resourceDirectory in Compile).value, (fullOptJS in Compile).value.data, dependencyFile.value, cssFile.value, (resourceDirectory in guiUtils in Compile).value / "css")
)

lazy val apiGUI = Project("apigui", file("apigui")) dependsOn (guiUtils) enablePlugins(ExecNpmPlugin) settings(
  scalaVersion := "2.12.8",
  buildGUI := guiBuilder(target.value, (resourceDirectory in Compile).value, (fullOptJS in Compile).value.data, dependencyFile.value, cssFile.value, (resourceDirectory in guiUtils in Compile).value / "css")
)