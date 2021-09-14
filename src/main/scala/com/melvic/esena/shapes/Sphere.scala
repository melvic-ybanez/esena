package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.rays.{Intersection, Ray}
import com.melvic.esena.tuples.Point

trait Sphere extends Shape {

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
    withTransformation(transformation * this.transformation)

  def withMaterial(newMaterial: Material): Sphere =
    Sphere(newMaterial, transformation)

  def withTransformation(newTransformation: Matrix): Sphere =
    Sphere(material, newTransformation)
}

object Sphere extends Sphere {
  def apply(): Sphere = new Sphere {}

  def apply(initMaterial: Material, initTransformation: Matrix): Sphere =
    new Sphere {
      override def material = initMaterial

      override def transformation = initTransformation
    }
}
