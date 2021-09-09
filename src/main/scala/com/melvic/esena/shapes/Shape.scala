package com.melvic.esena.shapes

import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.rays.Ray

trait Shape {
  def intersect(ray: Ray): Intersections
}
