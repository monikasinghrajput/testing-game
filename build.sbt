/*
* *********************************************************************************************************************************************************************
* Copyright (c) 2017 Tykhe Gaming Private Limited - All Rights Reserved

Licensed under the Software License Agreement (the "License") of Tykhe Gaming Private Limited.
You may not use this file except in compliance with the License.
You may obtain a copy of the License at http://tykhegaming.github.io/LICENSE.txt.

NOTICE
ALL INFORMATION CONTAINED HEREIN IS, AND REMAINS THE PROPERTY OF TYKHE GAMING PRIVATE LIMITED.
THE INTELLECTUAL AND TECHNICAL CONCEPTS CONTAINED HEREIN ARE PROPRIETARY TO TYKHE GAMING PRIVATE LIMITED AND ARE PROTECTED BY TRADE SECRET OR COPYRIGHT LAW.
DISSEMINATION OF THIS INFORMATION OR REPRODUCTION OF THIS MATERIAL IS STRICTLY FORBIDDEN UNLESS PRIOR WRITTEN PERMISSION IS OBTAINED FROM TYKHE GAMING PRIVATE LIMITED.

* **********************************************************************************************************************************************************************
* Change History
* **********************************************************************************************************************************************************************
* |     Date      |     Name     |      Change     |      Details
* |  01/08/2022   | Wilson Sam   |     Created     |  First Milestone
* |  21/11/2023   | Wilson Sam   |     Version     |  Packaged For Demo
* |  20/06/2024   | Wilson Sam   |     Version     |  Unity APK Support Initial Version
* **********************************************************************************************************************************************************************
* */

name := """poker-webapp-server"""
organization := "com.tykhe.table.poker"
maintainer := "wilson.sam@tykhegaming.com"
version := "3.0.0"
scalaVersion := "2.13.6"

lazy val root = (project in file(".")).
  enablePlugins(PlayScala)

pipelineStages := Seq(digest)

libraryDependencies ++= Seq(
  jdbc,
  "com.softwaremill.macwire" %% "macros" % "2.3.3" % "provided",
  "org.postgresql" % "postgresql" % "42.2.18",
  "org.scalikejdbc" %% "scalikejdbc" % "3.5.0",
  "org.scalikejdbc" %% "scalikejdbc-config"  % "3.5.0",
  "ch.qos.logback"  %  "logback-classic" % "1.2.3",
  "de.svenkubiak" % "jBCrypt" % "0.4.1",
  "com.lihaoyi" %% "os-lib" % "0.7.8",
  "com.lihaoyi" %% "upickle" % "1.4.0",
  "net.virtual-void" %%  "json-lenses" % "0.6.2",
  "ai.x" %% "play-json-extensions" % "0.42.0"
)

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.6"


sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false

PlayKeys.devSettings += "play.server.http.port" -> "9000"
PlayKeys.devSettings += "play.server.http.idleTimeout" -> "60000s"

//PlayKeys.devSettings += "play.server.https.port" -> "8080"
//PlayKeys.devSettings += "play.crypto.secret" -> "changethissosomethingsecret"
//PlayKeys.devSettings += "play.server.https.keyStore.path" ->  "/home/tykhe/i-gaming.online.jks"
//PlayKeys.devSettings += "play.server.https.keyStore.password" -> "HyMc34"
//PlayKeys.devSettings += "play.server.https.keyStore.type" -> "JKS"
