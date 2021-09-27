package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{Intersection, Ray}
import com.melvic.esena.shapes.Sphere.SphereImpl
import com.melvic.esena.tuples.Point

trait Sphere extends LeafShape[Sphere] {

  /**
    * The ray-sphere intersection is based on the
    * formula for finding the discriminant. If the
    * discriminant is negative, then there is no
    * intersection. Otherwise, the ray touches the
    * sphere in either one or two places.
    */
  override def localIntersect(transformedRay: Ray): Intersections = {
    // sphere is centered at the world origin
    val sphereToRay = transformedRay.origin - Point(0, 0, 0)

    val a = transformedRay.direction.dot(transformedRay.direction)
    val b = 2 * transformedRay.direction.dot(sphereToRay)
    val c = sphereToRay.dot(sphereToRay) - 1

    val discriminant = math.pow(b, 2) - 4 * a * c
    if (discriminant < 0) Vector()
    else
      Intersection.aggregate(
        Intersection((-b - math.sqrt(discriminant)) / (2 * a), this),
        Intersection((-b + math.sqrt(discriminant)) / (2 * a), this)
      )
  }

  override def localNormalAt(objectPoint: Point) =
    objectPoint - Point.Origin

  override def fromData(data: Shape.Data) =
    SphereImpl(data.material, data.transformation)
}

object Sphere extends Sphere {
  case class SphereImpl(override val material: Material, override val transformation: Matrix) extends Sphere

  val Glass: Sphere =
    Sphere.updateMaterial(_.copy(transparency = 1.0, refractiveIndex = 1.5))
}
