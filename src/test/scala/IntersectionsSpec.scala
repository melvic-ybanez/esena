import com.melvic.esena.rays.{Computations, Intersection, Intersections, Ray}
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class IntersectionsSpec extends AnyFlatSpec with should.Matchers {
  "An intersection" should "encapsulates t and object" in {
    val s = Sphere()
    val i = Intersection(3.5, s)
    i.t should be (3.5)
    i.obj should be (s)
  }

  "Integrations" should "be able to aggregate" in {
    val s = Sphere()
    val i1 = Intersection(1, s)
    val i2 = Intersection(2, s)
    val xs = Intersection.aggregate(i1, i2)
    xs.size should be (2)
    xs(0).t should be (1)
    xs(1).t should be (2)
  }

  "The hit of all positive intersections" should "have the lowest t" in {
    val s = Sphere()
    val i1 = Intersection(1, s)
    val i2 = Intersection(2, s)
    val xs = Intersections(i2, i1)
    Intersection.getHit(xs) should be(i1)
  }

  "The hit, when some intersections have negative t" should "have a positive t" in {
    val s = Sphere()
    val i1 = Intersection(-1, s)
    val i2 = Intersection(1, s)
    val xs = Intersections(i2, i1)
    Intersection.getHit(xs) should be (i2)
  }

  "The hit, when all intersections have negative t" should "be nothing" in {
    val s = Sphere()
    val i1 = Intersection(-2, s)
    val i2 = Intersection(-1, s)
    val xs = Intersections(i2, i1)
    Intersection.hit(xs) should be (None)
  }

  "The hit" should "always have the lowest non-negative intersection" in {
    val s = Sphere()
    val i1 = Intersection(5, s)
    val i2 = Intersection(7, s)
    val i3 = Intersection(-3, s)
    val i4 = Intersection(2, s)
    val xs = Intersections(i1, i2, i3, i4)
    Intersection.getHit(xs) should be (i4)
  }

  "Preparing the state of an intersection" should "return the precomputed information" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere()
    val i = Intersection(4, s)
    val comp = Computations.prepare(i, r)
    comp.t should be (i.t)
    comp.obj should be (i.obj)
    comp.point should be (Point(0, 0, -1))
    comp.eyeVec should be (Vec(0, 0, -1))
    comp.normalVec should be (Vec(0, 0, -1))
  }

  "The hit, when an intersection occurs on the outside" should "set the computations inside to false" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val shape = Sphere()
    val i = Intersection(4, shape)
    assert(!Computations.prepare(i, r).inside)
  }

  "The hit, when an intersection occurs on the outside" should "invert the normal vector" in {
    val r = Ray(Point.Origin, Vec(0, 0, 1))
    val shape = Sphere()
    val i = Intersection(1, shape)
    val comps = Computations.prepare(i, r)
    comps.point should be (Point(0, 0, 1))
    comps.eyeVec should be (Vec(0, 0, -1))
    assert(comps.inside)
    comps.normalVec should be (Vec(0, 0, -1))
  }
}
