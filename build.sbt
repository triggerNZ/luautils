name := "luautils"

scalaVersion := "2.11.7"

organization := "com.pavlinic"

licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))

libraryDependencies ++= Seq(
	"org.luaj" % "luaj-jse" % "3.0.1",
  "com.chuusai" %% "shapeless" % "2.3.0-SNAPSHOT",
	"org.specs2" %% "specs2-core" % "3.7" % Test)
