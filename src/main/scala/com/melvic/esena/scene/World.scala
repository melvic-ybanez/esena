package com.melvic.esena.scene

import com.melvic.esena.canvas.Color
import com.melvic.esena.lights
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.scaling
import com.melvic.esena.rays.{CanIntersect, Computations, Intersection, Ray}
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

  def addObjects(obj: Shape*): World =
    copy(objects = obj.toVector)

  override def intersect(ray: Ray) = {
    val intersections = objects.foldLeft(Vector.empty[Intersection]) { (acc, obj) =>
      acc ++ obj.intersect(ray)
    }
    intersections.sortBy(_.t)
  }

  def colorAt(ray: Ray): Color = {
    val hit = Intersection.hit(intersect(ray))
    hit.fold(Color.Black)(h => lights.shadeHit(this, Computations.prepare(h, ray)))
  }

  def isShadowedAt(point: Point): Boolean =
    light.fold(false) { light =>
      // vector from point to the light source
      val pointToLight = light.position - point

      val distance  = pointToLight.magnitude
      val direction = pointToLight.normalize

      // create a ray from the point to the light source
      val pointToLightRay = Ray(point, direction)

      val hit = Intersection.hit(intersect(pointToLightRay))
      hit.exists(_.t < distance)
    }
}

object World {
  lazy val default: World =
    World(
      Some(PointLight(Point(-10, 10, -10), Color.White)),
      Vector(
        Sphere.withMaterial(Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2)),
        Sphere.withTransformation(scaling(0.5, 0.5, 0.5))
      )
    )
}
