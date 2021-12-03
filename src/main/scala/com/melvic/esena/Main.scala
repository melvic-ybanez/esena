package com.melvic.esena

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object Main {
  def main(args: Array[String]): Unit = {
    println("Rendering...")
    val start = System.nanoTime()
    val canvas = MainScene.build
    val end = System.nanoTime()
    println(s"Rendering completed. Execution time: ${(end - start) / 1000 / 1000 / 1000} seconds")

    println(s"Writing to file...")
    val filename = "esena.ppm"
    Files.write(Paths.get(filename), canvas.ppm.toString.getBytes(StandardCharsets.UTF_8))
    println(s"Writing completed. Filename: $filename")
  }
}
