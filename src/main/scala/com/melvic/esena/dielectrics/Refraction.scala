package com.melvic.esena.dielectrics

import com.melvic.esena.Real

object Refraction {
  final case class RefractiveIndices(n1: Real, n2: Real)

  object index {
    val Vacuum = 1.0
    val Air = 1.00029
    val Water = 1.333
    val Glass = 1.52
    val Diamond = 2.417
    val Default = Vacuum
  }
}
