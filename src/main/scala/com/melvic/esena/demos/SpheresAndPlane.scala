package com.melvic.esena.demos

import com.melvic.esena.canvas.{Canvas, Color}
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.{scaling, translation}
import com.melvic.esena.scene.World
import com.melvic.esena.shapes.{Plane, Sphere}
import com.melvic.esena.tuples.Point

object SpheresAndPlane {
  def build: Canvas = {
    val floor = Plane()
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
      .copy(objects = Vector(floor, middleSphere, leftSphere) ++ moreSmallSpheres)

    DefaultCamera.render(world)
  }
}
