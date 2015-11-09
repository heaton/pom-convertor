package com.heaton.maven

import java.io.File
import java.net.URI
import java.nio.charset.Charset

import scala.io.Source

object Converter {

  case class Dependency(groupId: String, name: String, version: String) {
    private def makeXmlPattern:String = s"""<groupId>.*?</groupId>(\\s*)<artifactId>$name-$version</artifactId>(\\s*)<version>\\$$\\{.*?\\}</version>"""

    private def makeStandardXml: String = s"""<groupId>${groupId}</groupId>$$1<artifactId>${name}</artifactId>$$2<version>${version}</version>"""

    def replaceAll(src:String): String = src.replaceAll(makeXmlPattern, makeStandardXml)
  }

  val dependencyPattern = "(.*?):(.*?):(?:jar:)?(.*)".r

  def fixPom(publicJarsFile: String, pomFile: String): String = {
    val deps = getPublicJarsFromFile(publicJarsFile)
    val pom = readFile(pomFile).mkString
    deps.foldLeft(pom)(replaceAll)
  }

  private def replaceAll(pom:String, dependency: Dependency) = dependency.replaceAll(pom)

  def parse(input: String): Option[Dependency] = for {
    dependencyPattern(groupId, name, version) <- dependencyPattern.findFirstIn(input)
  } yield Dependency(groupId, name, version)

  def getPublicJarsFromFile(file: String): List[Dependency] = (for {
    line <- readFile(file).getLines
    dependency <- parse(line)
  } yield dependency).toList

  def saveFile(content: String, file: String): File = {
    import java.nio.file._
    val path: Path = Paths.get(file)
    Files.write(path, content.getBytes("UTF-8"))
    path.toFile
  }

  private def readFile(path: String) = Source fromFile uri(path)

  private def uri(path: String): URI = this.getClass.getResource(path).toURI

}

