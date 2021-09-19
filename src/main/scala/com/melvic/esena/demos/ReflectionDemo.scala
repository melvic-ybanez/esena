package com.melvic.esena.demos

import com.melvic.esena.canvas.{Canvas, Color}
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.{scaling, translation}
import com.melvic.esena.patterns.{CheckersPattern, GradientPattern}
import com.melvic.esena.shapes.{Plane, Sphere}

object ReflectionDemo {
  def build: Canvas = {
    val floor = Plane.withMaterial(
      Material(
        pattern = Some(CheckersPattern(Color.White, Color(0.5, 0.5, 0.5))),
        reflective = 0.2
      )
    )
    val middleSphere = Sphere
      .withTransformation(translation(-0.5, 1, 0.5))
      .withMaterial(
        Material(
          color = Color(0.1, 1, 0.5),
          diffuse = 0.7,
          specular = 0.3,
          pattern = Some(MainSpherePattern),
          reflective = 0.5
        )
      )
    val leftSphere = Sphere
      .withTransformation(translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33))
      .withMaterial(
        Material(
          diffuse = 0.7,
          specular = 0.3,
          pattern = Some(LeftSpherePattern),
          reflective = 0.5
        )
      )
    val rightSphere = Sphere
      .withTransformation(translation(1.1, 1, 0.7) * scaling(0.5, 0.5, 0.5))
      .withMaterial(
        Material(color = Color(1, 0.5, 0.5), diffuse = 0.7, specular = 0.3, reflective = 0.5)
      )

    val moreSmallSpheres = (0 until 5).map { i =>
      val componentScale = 0.5 + 0.1 * i
      val pattern        = GradientPattern(Color(1, 0.8, 0.1), Color(220.0 / 255, 20.0 / 255, 60.0 / 255))
      leftSphere
        .transform(translation(i, 0, 0) * scaling(componentScale, componentScale, componentScale))
        .withMaterial(
          Material(
            color = Color(0.5, 0.6, 1),
            diffuse = 0.7,
            specular = 0.3,
            pattern = if (i % 2 == 0) Some(pattern) else None,
            reflective = 0.5
          )
        )
    }

    defaultCanvas(Vector(floor, middleSphere, leftSphere, rightSphere) ++ moreSmallSpheres)
  }
}
