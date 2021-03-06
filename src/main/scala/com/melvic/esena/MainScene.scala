package com.melvic.esena

import com.melvic.esena.canvas.{Canvas, Color}
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.{scaling, translation, view}
import com.melvic.esena.patterns.{CheckersPattern, GradientPattern, RingPattern, StripePattern}
import com.melvic.esena.scene.{Camera, World}
import com.melvic.esena.shapes._
import com.melvic.esena.tuples.{Point, Vec}

object MainScene {
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
          pattern = Some(
            CheckersPattern(Color(21.0 / 255, 184.0 / 255, 0), Color(0.1, 1, 0.5))
              .scale(0.25, 0.25, 0.25)
              .rotateY(-math.Pi / 4)),
          reflective = 0.5
        )
      )

    val leftSphere = Sphere
      .withTransformation(translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33))
      .withMaterial(
        Material(
          diffuse = 0.7,
          specular = 0.3,
          pattern = Some(
            RingPattern(Color(1, 0.8, 0.1), Color.White)
              .scale(0.33, 0.33, 0.33)
              .rotateX(-math.Pi / 4)),
          reflective = 0.5
        )
      )

    val rightCube = Cube
      .scale(0.7, 0.7, 0.7)
      .rotateY(math.Pi / 4)
      .translate(1.1, 0.7, 3)
      .withMaterial(
        Material(
          diffuse = 0.7,
          specular = 0.3,
          pattern = Some(
            CheckersPattern(Color(1, 0.8, 0.1), Color.White)
              .scale(0.33, 0.33, 0.33)
              .rotateX(-math.Pi / 4)
          )
        )
      )

    val rightSphere = Sphere
      .withTransformation(translation(1.1, 2.1, 3) * scaling(0.7, 0.7, 0.7))
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

    canvas(Vector(floor, middleSphere, leftSphere, rightCube, rightSphere) ++ moreSmallSpheres ++ cylinders ++ cone)
  }

  def cylinders = {
    val colors = Vector(
      (40, 103, 160),
      (72, 120, 170),
      (99, 141, 187),
      (121, 158, 196),
      (157, 179, 208)
    )
    val offsetScale = 0.8
    val init = Vector(
      Cylinder
        .withMin(-0.1)
        .withMax(0.1)
        .updateMaterial(_.copy(color = Color(7.0 / 255, 87.0 / 255, 152.0 / 255)))
        .scale(offsetScale, 1, offsetScale)
        .translate(2, 0.1, 0.5))
    (0 until 5).foldLeft(init) { (acc, i) =>
      val last = acc.last
      val scaleFactor = {
        val scaleFactor = offsetScale - ((i + 1) * 0.2)
        if (scaleFactor >= 0.2) scaleFactor
        else offsetScale / math.pow(2, i)
      }
      val (r, g, b) = colors(i)
      val newCyl = Cylinder
        .withMin(last.min - 0.1)
        .withMax(last.max + 0.1)
        .updateMaterial(_.copy(color = Color(r / 255.0, g / 255.0, b / 255.0)))
        .scale(scaleFactor, 1, scaleFactor)
        .translate(2, last.max + 0.1, 0.5)
      acc :+ newCyl
    }
  }

  def cone = {
    val baseColor = Color(1, 168 / 255.0, 18 / 255.0)
    val cone = Cone
      .withMin(-1)
      .withMax(0)
      .withClosed(true)
      .withMaterial(
        Material(
          pattern = Some(
            StripePattern(Color.White, baseColor).scale(0.15).rotateZ(math.Pi / 2)
          )))
      .scale(0.5, 1.5, 0.5)
      .translate(-3.5, 1.6, 4.5)
    val base = Cylinder
      .withClosed(true)
      .withMaterial(Material(color = baseColor))
      .withMin(-0.1)
      .withMax(0.1)
      .scale(0.6, 1, 0.6)
      .translate(-3.5, 0.1, 4.5)
    Vector(base, cone)
  }

  def canvas(objects: Vector[Shape]) = {
    // white light source, from above and to the left
    val world = World.default
      .withLight(PointLight(Point(-10, 12, -10), Color.White))
      .copy(objects = objects)

    val camera = Camera(1000, 600, math.Pi / 3)
      .transform(view(Point(0, 1.5, -5), Point(0, 1, 0), Vec(0, 1, 0)))

    camera.render(world)
  }
}
