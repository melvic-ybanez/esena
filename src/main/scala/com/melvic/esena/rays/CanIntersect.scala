package com.melvic.esena.rays

import com.melvic.esena.rays.Intersections.Intersections

trait CanIntersect {
  def intersect(ray: Ray): Intersections
}
