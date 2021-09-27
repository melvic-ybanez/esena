package com.melvic.esena

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object Main {
  def main(args: Array[String]): Unit = {
    val canvas = MainScene.build

    // write to file
    Files.write(Paths.get("esena.ppm"), canvas.ppm.toString.getBytes(StandardCharsets.UTF_8))
  }
}
