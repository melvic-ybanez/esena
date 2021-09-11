package com.melvic.esena.scene

import com.melvic.esena.canvas.Color
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.scaling
import com.melvic.esena.rays.{CanIntersect, Intersection, Ray}
import com.melvic.esena.shapes.{Shape, Sphere}
import com.melvic.esena.tuples.Point

final case class World(
    light: Option[PointLight] = None,
    objects: Vector[Shape] = Vector()
) extends CanIntersect {
  def contains(shape: Shape): Boolean =
    objects.contains(shape)

  def withLight(light: PointLight): World =
    copy(light = Some(light))

  override def intersect(ray: Ray) = {
    val intersections = objects.foldLeft(Vector.empty[Intersection]) { (acc, obj) =>
      acc ++ obj.intersect(ray)
    }
    intersections.sortBy(_.t)
  }
}

object World {
  lazy val default: World =
    World(
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
