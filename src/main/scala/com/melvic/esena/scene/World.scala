package com.melvic.esena.scene

import com.melvic.esena.canvas.Color
import com.melvic.esena.{lights, reflections}
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
    copy(objects = objects ++ obj)

  def updateObject(index: Int, shape: Shape): World =
    copy(objects = objects.updated(index, shape))

  def updateObjectWith(index: Int)(f: Shape => Shape): World =
    updateObject(index, f(objects(index)))

  override def intersect(ray: Ray) = {
    val intersections = objects.foldLeft(Vector.empty[Intersection]) { (acc, obj) =>
      acc ++ obj.intersect(ray)
    }
    intersections.sortBy(_.t)
  }

  def colorAt(ray: Ray, depth: Int = reflections.DefaultDepth): Color = {
    val xs  = intersect(ray)
    val hit = Intersection.hit(xs)
    hit.fold(Color.Black)(h => lights.shadeHit(this, Computations.prepare(h, ray, xs), depth))
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

  def refractedColor(comps: Computations, depth: Int): Color =
    if (depth == 0 || comps.obj.material.transparency == 0)
      Color.Black
    else {
      // According to Snell's Law, sin(theta_i) / sin(theta_t) = n2 / n1.
      // In our case, theta_i is the angle of the incoming ray and theta_t
      // is the angle of the refracted ray.
      val nRatio = comps.n1 / comps.n2
      val cosI   = comps.eyeVec.dot(comps.normalVec)

      // Pythagorean identity: sin^2 (t) + cos^2 (t) = 1
      val sin2T = nRatio * nRatio * (1.0 - cosI * cosI)

      val isTotalInternalReflection = sin2T > 1
      if (isTotalInternalReflection) Color.Black
      else {
        // again, from Pythagorean identities
        val cosT = math.sqrt(1.0 - sin2T)
        // direction of the refracted ray
        val direction = comps.normalVec * (nRatio * cosI - cosT) - comps.eyeVec * nRatio

        val refractedRay = Ray(comps.underPoint, direction)
        colorAt(refractedRay, depth - 1) * comps.obj.material.transparency
      }
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
