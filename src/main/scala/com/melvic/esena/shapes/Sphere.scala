package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.rays.{Intersection, Ray}
import com.melvic.esena.tuples.{Point, Vec}

final case class Sphere(transformation: Matrix, material: Material) extends Shape {

  /**
    * The ray-sphere intersection is based on the
    * formula for finding the discriminant. If the
    * discriminant is negative, then there is no
    * intersection. Otherwise, the ray touches the
    * sphere in either one or two places.
    */
  override def intersect(ray: Ray): Intersections = {
    // To maintain the sphere's center at the origin and its radius
    // to 1, we transform the ray by the inverse of the sphere's
    // transformation instead.
    val transformedRay = ray.transform(transformation.inverse)

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

  def transform(transformation: Matrix): Sphere =
    copy(transformation = transformation)

  def normalAt(worldPoint: Point): Vec = {
    val objectPoint = transformation.inverse * worldPoint
    val objectNormal = objectPoint - Point.Origin
    val worldNormal = transformation.inverse.transpose * objectNormal
    val worldNormalVec = worldNormal.toVec   // sets the w to 0
    worldNormalVec.normalize
  }

  def withMaterial(material: Material): Sphere =
    copy(material = material)
}

object Sphere {
  def apply(): Sphere = new Sphere(Matrix.Identity4x4, Material())

  def apply(transformation: Matrix): Sphere =
    Sphere().transform(transformation)
}
