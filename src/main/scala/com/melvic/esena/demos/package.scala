package com.melvic.esena

import com.melvic.esena.canvas.Color
import com.melvic.esena.lights.PointLight
import com.melvic.esena.matrix.view
import com.melvic.esena.patterns.{CheckersPattern, RingPattern}
import com.melvic.esena.scene.{Camera, World}
import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.{Point, Vec}

package object demos {
  val DefaultCamera = Camera(1000, 600, math.Pi / 3)
    .transform(view(Point(0, 1.5, -5), Point(0, 1, 0), Vec(0, 1, 0)))

  def defaultCanvas(objects: Vector[Shape]) = {
    // white light source, from above and to the left
    val world = World.default
      .withLight(PointLight(Point(-10, 10, -10), Color.White))
      .copy(objects = objects)

    DefaultCamera.render(world)
  }

  val MainSpherePattern = CheckersPattern(Color(21.0 / 255, 184.0 / 255, 0), Color(0.1, 1, 0.5))
    .scale(0.25, 0.25, 0.25)
    .rotateY(-math.Pi / 4)

  val LeftSpherePattern = RingPattern(Color(1, 0.8, 0.1), Color.White)
    .scale(0.33, 0.33, 0.33)
    .rotateX(-math.Pi / 4)
}
