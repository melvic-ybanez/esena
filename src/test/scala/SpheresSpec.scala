import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.matrix._
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SpheresSpec extends AnyFlatSpec with should.Matchers {
  "A ray that intersects a sphere at two points" should "yield 2 different values" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere()
    val xs = s.intersect(r)
    xs(0).t should be (4.0)
    xs(1).t should be (6.0)
  }

  "A ray that intersects a sphere at a tangent" should "yield 2 same values" in {
    val r = Ray(Point(0, 1, -5), Vec(0, 0, 1))
    val s = Sphere()
    val xs = s.intersect(r)
    xs(0).t should be (5.0)
    xs(1).t should be (5.0)
  }

  "A ray that misses the sphere" should "not yield any values" in {
    val r = Ray(Point(0, 2, -5), Vec(0, 0, 1))
    val s = Sphere()
    val xs = s.intersect(r)
    assert(xs.isEmpty)
  }

  "A ray originating inside a sphere" should "have one of the intersections behind its origin" in {
    val r = Ray(Point(0, 0, 0), Vec(0, 0, 1))
    val s = Sphere()
    val xs = s.intersect(r)
    xs.size should be (2)
    xs(0).t should be (-1.0)
    xs(1).t should be (1.0)
  }

  "A sphere behind a ray" should "intersect the ray, resulting to negative t values" in {
    val r = Ray(Point(0, 0, 5), Vec(0, 0, 1))
    val s = Sphere()
    val xs = s.intersect(r)
    xs.size should be (2)
    xs(0).t should be (-6.0)
    xs(1).t should be (-4.0)
  }

  "Intersection with sphere" should "set the sphere as the object of intersection" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere()
    val xs = s.intersect(r)
    xs.size should be (2)
    xs(0).obj should be (s)
    xs(1).obj should be (s)
  }

  "A sphere" should "have a default transformation of the identity matrix" in  {
    val s = Sphere()
    s.transformation should be (Identity4x4)
  }

  "A sphere" should "be able to update transformation" in {
    val s = Sphere()
    val t = translation(2, 3, 4)
    s.transform(t).transformation should be (t)
  }

  "Intersecting a scaled sphere with a ray" should "work by scaling the ray inversely" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere(scaling(2, 2, 2))
    val xs = s.intersect(r)
    xs.size should be (2)
    xs(0).t should be (3)
    xs(1).t should be (7)
  }

  "Intersecting a translated sphere with a ray" should "work by translating the ray inversely" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere(translation(5, 0, 0))
    val xs = s.intersect(r)
    xs.size should be (0)
  }
}
