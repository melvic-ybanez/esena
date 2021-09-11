package com.melvic.esena.rays

import com.melvic.esena.rays.Intersection.Intersections

trait CanIntersect {
  def intersect(ray: Ray): Intersections
}
