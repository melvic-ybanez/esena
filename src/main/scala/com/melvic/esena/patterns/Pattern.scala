package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color

sealed trait Pattern

object Pattern {
  case object NoPattern                                       extends Pattern
  final case class StripePattern(first: Color, second: Color) extends Pattern
}
