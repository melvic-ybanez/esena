package com.melvic.esena

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.melvic.esena.canvas.{Canvas, Color}
import com.melvic.esena.lights.{Material, PointLight, lighting}
import com.melvic.esena.matrix.{rotationX, rotationY, scaling, translation, view}
import com.melvic.esena.rays.{Intersection, Ray}
import com.melvic.esena.scene.{Camera, World}
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.{Point, Vec}

object Main {
  def main(args: Array[String]): Unit = {
    // This is just a sample rendered world for now
    val floor = Sphere()
      .withTransformation(scaling(10, 0.01, 10))
      .withMaterial(Material(color = Color(1, 0.9, 0.9), specular = 0))
    val leftWall = Sphere()
      .withTransformation(translation(0, 0, 5) * rotationY(-math.Pi / 4) * rotationX(math.Pi / 2) * scaling(10, 0.01, 10))
      .withMaterial(floor.material)
    val rightWall = Sphere()
      .withTransformation(translation(0, 0, 5) * rotationY(math.Pi / 4) * rotationX(math.Pi / 2) * scaling(10, 0.01, 10))
      .withMaterial(floor.material)
    val middleSphere = Sphere()
      .withTransformation(translation(-0.5, 1, 0.5))
      .withMaterial(Material(color = Color(0.1, 1, 0.5), diffuse = 0.7, specular = 0.3))
    val leftSphere = Sphere()
      .withTransformation(translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33))
      .withMaterial(Material(color = Color(1, 0.8, 0.1), diffuse = 0.7, specular = 0.3))

    val moreSmallSpheres = (0 until 5).map { i =>
      val componentScale = 0.5 + 0.1 * i
      leftSphere.transform(translation(i, 0, 0) * scaling(componentScale, componentScale, componentScale))
    }

    // white light source, from above and to the left
    val world = World.default.withLight(PointLight(Point(-10, 10, -10), Color.White))
      .copy(objects = Vector(floor, leftWall, rightWall, middleSphere, leftSphere) ++ moreSmallSpheres)

    val cam = Camera(1000, 600, math.Pi / 3).transform(view(Point(0, 1.5, -5), Point(0, 1, 0), Vec(0, 1, 0)))
    val canvas = cam.render(world)

    // write to file
    Files.write(Paths.get("sample.ppm"), canvas.ppm.toString.getBytes(StandardCharsets.UTF_8))
  }
}
