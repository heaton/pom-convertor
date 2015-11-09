package me.heaton.spec2

import com.heaton.maven.Converter
import org.specs2._

import scala.io.Source

class ConverterSpec extends mutable.Specification {

  import Converter._

  "parse input" in {
    parse("groupId:name:jar:1.0.0") === Some(Dependency("groupId", "name", "1.0.0"))
    parse("groupId:name:1.0.0") === Some(Dependency("groupId", "name", "1.0.0"))
    parse("com.apache:common-io:2.4.7") === Some(Dependency("com.apache", "common-io", "2.4.7"))
  }

  "get public dependencies from a file" in {
    getPublicJarsFromFile("/public-jars.txt") === List(Dependency("g1", "n1", "v1"), Dependency("g2", "n2", "v2"))
  }

  "Dependency" should {
    "has replaceAll" in {
      "fix a wrong dependency" in {
        val srcDepency =
          """
            |        <dependency>
            |            <groupId>com.other.wrong</groupId>
            |            <artifactId>logback-classic-1.0.13</artifactId>
            |            <version>${not-real}</version>
            |        </dependency>
          """.
            stripMargin.trim
        Dependency("ch.qos.logback", "logback-classic", "1.0.13").replaceAll(srcDepency) ===
          """
            |        <dependency>
            |            <groupId>ch.qos.logback</groupId>
            |            <artifactId>logback-classic</artifactId>
            |            <version>1.0.13</version>
            |        </dependency>
          """.
            stripMargin.trim
      }

      "fix a wrong dependency with a scope" in {
        val srcDepency =
          """
            |        <dependency>
            |            <groupId>com.other.wrong</groupId>
            |            <artifactId>logback-classic-1.0.13</artifactId>
            |            <version>${not-real}</version>
            |            <scope>provided<scope>
            |        </dependency>
          """.
            stripMargin.trim
        Dependency("ch.qos.logback", "logback-classic", "1.0.13").replaceAll(srcDepency) ===
          """
            |        <dependency>
            |            <groupId>ch.qos.logback</groupId>
            |            <artifactId>logback-classic</artifactId>
            |            <version>1.0.13</version>
            |            <scope>provided<scope>
            |        </dependency>
          """.
            stripMargin.trim
      }

      "fix more than one wrong dependency" in {
        val srcDepency =
          """
            |        <dependency>
            |            <groupId>com.other.wrong</groupId>
            |            <artifactId>logback-classic-1.0.13</artifactId>
            |            <version>${not-real}</version>
            |        </dependency>
            |        <dependency>
            |            <groupId>com.other.wrong</groupId>
            |            <artifactId>httpclient-4.2.5</artifactId>
            |            <version>${not-real}</version>
            |        </dependency>
            |        <dependency>
            |            <groupId>com.other.wrong</groupId>
            |            <artifactId>logback-classic-1.0.13</artifactId>
            |            <version>${not-real}</version>
            |        </dependency>
          """.
            stripMargin.trim
        Dependency("ch.qos.logback", "logback-classic", "1.0.13").replaceAll(srcDepency) ===
          """
            |        <dependency>
            |            <groupId>ch.qos.logback</groupId>
            |            <artifactId>logback-classic</artifactId>
            |            <version>1.0.13</version>
            |        </dependency>
            |        <dependency>
            |            <groupId>com.other.wrong</groupId>
            |            <artifactId>httpclient-4.2.5</artifactId>
            |            <version>${not-real}</version>
            |        </dependency>
            |        <dependency>
            |            <groupId>ch.qos.logback</groupId>
            |            <artifactId>logback-classic</artifactId>
            |            <version>1.0.13</version>
            |        </dependency>
          """.
            stripMargin.trim
      }
    }
  }

  "fix pom" in {
    fixPom("/public-jars-test.txt", "/pom-test.xml").trim ===
      """
        |<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0"
        |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |    <modelVersion>4.0.0</modelVersion>
        |
        |    <groupId>com.heaton</groupId>
        |    <artifactId>java-demo</artifactId>
        |    <version>1.0-SNAPSHOT</version>
        |
        |    <properties>
        |        <me.version>1.0</me.version>
        |    </properties>
        |
        |    <dependencies>
        |        <dependency>
        |            <groupId>ch.qos.logback</groupId>
        |            <artifactId>logback-classic</artifactId>
        |            <version>1.0.13</version>
        |        </dependency>
        |        <dependency>
        |            <groupId>org.seleniumhq.selenium</groupId>
        |            <artifactId>selenium-java</artifactId>
        |            <version>2.48.2</version>
        |        </dependency>
        |        <dependency>
        |            <groupId>junit</groupId>
        |            <artifactId>junit</artifactId>
        |            <version>4.12</version>
        |            <scope>test</scope>
        |        </dependency>
        |    </dependencies>
        |
        |</project>
      """.stripMargin.trim
  }

  "wirte file" in {
    val filePath = "test.txt"
    val content = "Hello World!"
    val file = saveFile(content, filePath)
    Source.fromFile(file).mkString === content
  }

}
