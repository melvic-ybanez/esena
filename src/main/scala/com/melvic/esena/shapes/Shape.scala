package com.melvic.esena.shapes

import com.melvic.esena.lights.Material
import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.rays.Ray
import com.melvic.esena.tuples.{Point, Vec}

trait Shape {
  def material: Material

  def intersect(ray: Ray): Intersections

  def normalAt(point: Point): Vec
}
