import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SpheresSpec extends AnyFlatSpec with should.Matchers {
  "A ray that intersects a sphere at two points" should "yield 2 different values" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = new Sphere()
    val xs = s.intersect(r)
    xs(0) should be (4.0)
    xs(1) should be (6.0)
  }

  "A ray that intersects a sphere at a tangent" should "yield 2 same values" in {
    val r = Ray(Point(0, 1, -5), Vec(0, 0, 1))
    val s = new Sphere
    val xs = s.intersect(r)
    xs(0) should be (5.0)
    xs(1) should be (5.0)
  }

  "A ray that misses the sphere" should "not yield any values" in {
    val r = Ray(Point(0, 2, -5), Vec(0, 0, 1))
    val s = new Sphere
    val xs = s.intersect(r)
    assert(xs.isEmpty)
  }

  "A ray originating inside a sphere" should "have one of the intersections behind its origin" in {
    val r = Ray(Point(0, 0, 0), Vec(0, 0, 1))
    val s = new Sphere
    val xs = s.intersect(r)
    xs.size should be (2)
    xs(0) should be (-1.0)
    xs(1) should be (1.0)
  }

  "A sphere behind a ray" should "intersect the ray, resulting to negative t values" in {
    val r = Ray(Point(0, 0, 5), Vec(0, 0, 1))
    val s = new Sphere
    val xs = s.intersect(r)
    xs.size should be (2)
    xs(0) should be (-6.0)
    xs(1) should be (-4.0)
  }
}
