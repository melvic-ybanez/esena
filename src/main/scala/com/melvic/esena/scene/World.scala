package com.melvic.esena.scene

import com.melvic.esena.canvas.Color
import com.melvic.esena.lights.Material.{DefaultAmbient, DefaultShininess}
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.scaling
import com.melvic.esena.shapes.{Shape, Sphere}
import com.melvic.esena.tuples.Point

trait World {
  def light: Option[PointLight]

  def objects: Vector[Shape]

  def contains(shape: Shape): Boolean =
    objects.contains(shape)
}

object World {
  final case class WorldImpl(light: Option[PointLight], objects: Vector[Shape]) extends World

  def apply(): World =
    WorldImpl(None, Vector())

  lazy val default: World =
    WorldImpl(
      Some(PointLight(Point(-10, 10, -10), Color.White)),
      Vector(
        Sphere(
          material = Material(
            color = Color(0.8, 1.0, 0.6),
            diffuse = 0.7,
            specular = 0.2,
          )),
        Sphere(transformation = scaling(0.5, 0.5, 0.5))
      )
    )
}
