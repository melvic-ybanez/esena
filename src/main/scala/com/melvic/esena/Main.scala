package com.melvic.esena

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.melvic.esena.demos.StripePatterns

object Main {
  def main(args: Array[String]): Unit = {
    // we are choosing one of the demos for now
    val canvas = StripePatterns.build

    // write to file
    Files.write(Paths.get("sample.ppm"), canvas.ppm.toString.getBytes(StandardCharsets.UTF_8))
  }
}
