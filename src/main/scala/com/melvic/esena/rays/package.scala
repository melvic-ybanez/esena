package com.melvic.esena

import com.melvic.esena.rays.Intersection.Intersections

package object rays {
  def Intersections(intersection: Intersection*): Intersections =
    Intersection.aggregate(intersection: _*)
}
