import com.melvic.esena.matrix.translation
import com.melvic.esena.MathUtils
import com.melvic.esena.MathUtils.{roundTo, roundTo5}
import com.melvic.esena.rays.{Computations, Intersection, Intersections, Ray}
import com.melvic.esena.shapes.{Plane, Sphere}
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class IntersectionsSpec extends AnyFlatSpec with should.Matchers {
  "An intersection" should "encapsulates t and object" in {
    val i = Intersection(3.5, Sphere)
    i.t should be (3.5)
    i.obj should be (Sphere)
  }

  "Integrations" should "be able to aggregate" in {
    val i1 = Intersection(1, Sphere)
    val i2 = Intersection(2, Sphere)
    val xs = Intersection.aggregate(i1, i2)
    xs.size should be (2)
    xs(0).t should be (1)
    xs(1).t should be (2)
  }

  "The hit of all positive intersections" should "have the lowest t" in {
    val i1 = Intersection(1, Sphere)
    val i2 = Intersection(2, Sphere)
    val xs = Intersections(i2, i1)
    Intersection.getHit(xs) should be(i1)
  }

  "The hit, when some intersections have negative t" should "have a positive t" in {
    val s = Sphere
    val i1 = Intersection(-1, s)
    val i2 = Intersection(1, s)
    val xs = Intersections(i2, i1)
    Intersection.getHit(xs) should be (i2)
  }

  "The hit, when all intersections have negative t" should "be nothing" in {
    val i1 = Intersection(-2, Sphere)
    val i2 = Intersection(-1, Sphere)
    val xs = Intersections(i2, i1)
    Intersection.hit(xs) should be (None)
  }

  "The hit" should "always have the lowest non-negative intersection" in {
    val i1 = Intersection(5, Sphere)
    val i2 = Intersection(7, Sphere)
    val i3 = Intersection(-3, Sphere)
    val i4 = Intersection(2, Sphere)
    val xs = Intersections(i1, i2, i3, i4)
    Intersection.getHit(xs) should be (i4)
  }

  "Preparing the state of an intersection" should "return the precomputed information" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val i = Intersection(4, Sphere)
    val comp = Computations.prepare(i, r)
    comp.t should be (i.t)
    comp.obj should be (i.obj)
    comp.point should be (Point(0, 0, -1))
    comp.eyeVec should be (Vec(0, 0, -1))
    comp.normalVec should be (Vec(0, 0, -1))
  }

  "The hit, when an intersection occurs on the outside" should "set the computations inside to false" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val shape = Sphere
    val i = Intersection(4, shape)
    assert(!Computations.prepare(i, r).inside)
  }

  "The hit, when an intersection occurs on the outside" should "invert the normal vector" in {
    val r = Ray(Point.Origin, Vec(0, 0, 1))
    val shape = Sphere
    val i = Intersection(1, shape)
    val comps = Computations.prepare(i, r)
    comps.point should be (Point(0, 0, 1))
    comps.eyeVec should be (Vec(0, 0, -1))
    assert(comps.inside)
    comps.normalVec should be (Vec(0, 0, -1))
  }

  "The hit" should "offset the point" in {
    val ray = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val shape = Sphere.transform(translation(0, 0, 1))
    val intersection = Intersection(5, shape)   // intersection at z=0
    val comps = Computations.prepare(intersection, ray)
    assert(comps.overPoint.z < -MathUtils.Epsilon / 2)
    assert(comps.point.z > comps.overPoint.z)
  }

  "Pre-computations" should "include the reflection vector of the ray" in {
    val shape = Plane
    val ray = Ray(Point(0, 1, -1), Vec(0, -math.sqrt(2) / 2, math.sqrt(2) / 2))
    val i = Intersection(math.sqrt(2), shape)
    val comps = Computations.prepare(i, ray)
    comps.reflectVec should be (Vec(0, math.sqrt(2) / 2, math.sqrt(2) / 2))
  }

  "Pre-computations" should "determine n1 and n2 correctly" in {
    val a = Sphere.Glass.scale(2, 2, 2).updateMaterial(_.copy(refractiveIndex = 1.5))
    val b = Sphere.Glass.translate(0, 0, -0.25).updateMaterial(_.copy(refractiveIndex = 2.0))
    val c = Sphere.Glass.translate(0, 0, 0.25).updateMaterial(_.copy(refractiveIndex = 2.5))
    val ray = Ray(Point(0, 0, -4), Vec(0, 0, 1))
    val xs = Intersections(
      Intersection(2, a),
      Intersection(2.75, b),
      Intersection(3.25, c),
      Intersection(4.75, b),
      Intersection(5.25, c),
      Intersection(6, a)
    )
    val data = Vector(1.0 -> 1.5, 1.5 -> 2.0, 2.0 -> 2.5, 2.5 -> 2.5, 2.5 -> 1.5, 1.5 -> 1.0)
    data.zipWithIndex.foreach { case ((n1, n2), i) =>
      val comps = Computations.prepare(xs(i), ray, xs)
      comps.refractive.n1 should be (n1)
      comps.refractive.n2 should be (n2)
    }
  }

  "The under point" should "be offset below the surface" in {
    val ray = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val shape = Sphere.translate(0, 0, 1)
    val i = Intersection(5, shape)
    val xs = Intersections(i)
    val comps = Computations.prepare(i, ray, xs)
    assert(comps.underPoint.z > MathUtils.Epsilon / 2)
    assert(comps.point.z < comps.underPoint.z)
  }

  "The Schlick approximation under total internal reflection" should "be 1" in {
    val shape = Sphere.Glass
    val ray = Ray(Point(0, 0, math.sqrt(2) / 2), Vec(0, 1, 0))
    val xs = Intersections(-math.sqrt(2) / 2 -> shape, math.sqrt(2) / 2 -> shape)
    val comps = Computations.prepare(xs(1), ray, xs)
    val reflectance = Intersections.schlick(comps)
    reflectance should be (1.0)
  }

  "The Schlick approximation with a perpendicular viewing angle" should "be small" in {
    val shape = Sphere.Glass
    val ray = Ray(Point.Origin, Vec(0, 1, 0))
    val xs = Intersections(-1.0 -> shape, 1.0 -> shape)
    val comps = Computations.prepare(xs(1), ray, xs)
    val reflectance = Intersections.schlick(comps)
    roundTo(2)(reflectance) should be (0.04)
  }

  "The Schlick approximation with small angle and n2 > n1" should "be significant" in {
    val shape = Sphere.Glass
    val ray = Ray(Point(0, 0.99, -2), Vec(0, 0, 1))
    val xs = Intersections(1.8589 -> shape)
    val comps = Computations.prepare(xs.head, ray, xs)
    val reflectance = Intersections.schlick(comps)
    roundTo5(reflectance) should be (0.48873)
  }
}
