package com.melvic.esena

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.melvic.esena.canvas.{Canvas, Color}
import com.melvic.esena.lights.{Material, PointLight, lighting}
import com.melvic.esena.rays.{Intersection, Ray}
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.Point

object Main {
  def main(args: Array[String]): Unit = {
    // This is just a sample rendered shape for now
    val rayOrigin = Point(0, 0, -5)
    val wallZ = 10
    val wallSize = 7.0
    val canvasPixels = 500
    val pixelSize = wallSize / canvasPixels
    val half = wallSize / 2

    // sample canvas
    val canvas = Canvas(canvasPixels, canvasPixels)
    val shape = Sphere().withMaterial(Material(color = Color(1, 0.2, 1)))

    val light = PointLight(Point(-10, 10, -10), Color.White)

    // draw the object on the canvas
    val updatedCanvas = (0 until canvasPixels).foldLeft(canvas) { (canvas, y) =>
      val worldY = half - pixelSize * y
      (0 until canvasPixels).foldLeft(canvas) { (canvas, x) =>
        val worldX = -half + pixelSize * x
        val position = Point(worldX, worldY, wallZ)

        val ray = Ray(rayOrigin, (position - rayOrigin).normalize)
        val xs = shape.intersect(ray)
        val hits = Intersection.hit(xs)

        if (hits.nonEmpty) {
          val hit = hits.head
          val point = ray.position(hit.t)
          val normal = hit.obj.normalAt(point)
          val eye = -ray.direction

          val color = lighting(hit.obj.material, light, point, eye, normal)

          canvas.writePixel(x, y, color)
        } else canvas
      }
    }

    println(updatedCanvas.ppm.toString)

    // write to file
    Files.write(Paths.get("sample.ppm"), updatedCanvas.ppm.toString.getBytes(StandardCharsets.UTF_8))
  }
}
