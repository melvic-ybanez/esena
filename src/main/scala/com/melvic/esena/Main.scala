package com.melvic.esena

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.melvic.esena.demos.{SpheresAndPlane, SpheresAndWalls}

object Main {
  def main(args: Array[String]): Unit = {
    // we are choosing one of the demos for now
    val canvas = SpheresAndPlane.build

    // write to file
    Files.write(Paths.get("sample.ppm"), canvas.ppm.toString.getBytes(StandardCharsets.UTF_8))
  }
}
