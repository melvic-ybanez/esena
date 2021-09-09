import com.melvic.esena.rays.{Intersection, Intersections}
import com.melvic.esena.shapes.Sphere
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
}
