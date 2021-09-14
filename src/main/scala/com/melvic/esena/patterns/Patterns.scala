package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.patterns.Pattern.StripePattern
import com.melvic.esena.tuples.Point

trait Patterns {
  def stripeAt(pattern: StripePattern, point: Point): Color =
    if (math.floor(point.x) % 2 == 0) pattern.first
    else pattern.second
}
