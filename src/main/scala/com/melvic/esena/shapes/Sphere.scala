package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.rays.{Intersection, Ray}
import com.melvic.esena.tuples.Point

final case class Sphere(
    transformation: Matrix = Matrix.Identity4x4,
    material: Material = Material()
) extends Shape {

  /**
    * The ray-sphere intersection is based on the
    * formula for finding the discriminant. If the
    * discriminant is negative, then there is no
    * intersection. Otherwise, the ray touches the
    * sphere in either one or two places.
    */
  override def intersectWithTransformedRay(transformedRay: Ray): Intersections = {
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

  /**
    * Adds a transformation on top of the already existing one
    */
  def transform(transformation: Matrix): Sphere =
    withTransformation(transformation = transformation * this.transformation)

  def withMaterial(material: Material): Sphere =
    copy(material = material)

  def withTransformation(transformation: Matrix): Sphere =
    copy(transformation = transformation)
}

object Sphere {
  def apply(transformation: Matrix): Sphere =
    Sphere().withTransformation(transformation)
}
