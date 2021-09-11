package com.melvic.esena.shapes

import com.melvic.esena.lights.Material
import com.melvic.esena.rays.{CanIntersect, Ray}
import com.melvic.esena.tuples.{Point, Vec}

trait Shape extends CanIntersect {
  def material: Material

  def normalAt(point: Point): Vec
}
