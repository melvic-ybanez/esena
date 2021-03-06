import com.melvic.esena.MathUtils.roundTo5
import com.melvic.esena.lights.Material
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
    val xs = Sphere.intersect(r)
    xs(0).t should be (4.0)
    xs(1).t should be (6.0)
  }

  "A ray that intersects a sphere at a tangent" should "yield 2 same values" in {
    val r = Ray(Point(0, 1, -5), Vec(0, 0, 1))
    val xs = Sphere.intersect(r)
    xs(0).t should be (5.0)
    xs(1).t should be (5.0)
  }

  "A ray that misses the sphere" should "not yield any values" in {
    val r = Ray(Point(0, 2, -5), Vec(0, 0, 1))
    val xs = Sphere.intersect(r)
    assert(xs.isEmpty)
  }

  "A ray originating inside a sphere" should "have one of the intersections behind its origin" in {
    val r = Ray(Point(0, 0, 0), Vec(0, 0, 1))
    val xs = Sphere.intersect(r)
    xs.size should be (2)
    xs(0).t should be (-1.0)
    xs(1).t should be (1.0)
  }

  "A sphere behind a ray" should "intersect the ray, resulting to negative t values" in {
    val r = Ray(Point(0, 0, 5), Vec(0, 0, 1))
    val xs = Sphere.intersect(r)
    xs.size should be (2)
    xs(0).t should be (-6.0)
    xs(1).t should be (-4.0)
  }

  "Intersection with sphere" should "set the sphere as the object of intersection" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val xs = Sphere.intersect(r)
    xs.size should be (2)
    xs(0).obj should be (Sphere)
    xs(1).obj should be (Sphere)
  }

  "A sphere" should "have a default transformation of the identity matrix" in  {
    Sphere.transformation should be (Identity4x4)
  }

  "A sphere" should "be able to update transformation" in {
    val t = translation(2, 3, 4)
    Sphere.transform(t).transformation should be (t)
  }

  "Intersecting a scaled sphere with a ray" should "work by scaling the ray inversely" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere.transform(scaling(2, 2, 2))
    val xs = s.intersect(r)
    xs.size should be (2)
    xs(0).t should be (3)
    xs(1).t should be (7)
  }

  "Intersecting a translated sphere with a ray" should "work by translating the ray inversely" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere.transform(translation(5, 0, 0))
    val xs = s.intersect(r)
    xs.size should be (0)
  }

  "The normal of a sphere at a point on the x axis" should "point along the x-axis" in {
    val n = Sphere.normalAt(Point(1, 0, 0))
    n should be (Vec(1, 0, 0))
  }

  "The normal of a sphere at a point on the y axis" should "point along the y-axis" in {
    val n = Sphere.normalAt(Point(0, 1, 0))
    n should be (Vec(0, 1, 0))
  }

  "The normal of a sphere at a point on the z axis" should "point along the z-axis" in {
    val n = Sphere.normalAt(Point(0, 0, 1))
    n should be (Vec(0, 0, 1))
  }

  "The normal of a sphere" should "be computable for any non-axial point" in {
    val sqrtOf3Over3 = math.sqrt(3) / 3
    val n = Sphere.normalAt(Point(sqrtOf3Over3, sqrtOf3Over3, sqrtOf3Over3))
    n should be (Vec(sqrtOf3Over3, sqrtOf3Over3, sqrtOf3Over3))
  }

  "The normal of a sphere" should "be a normalized vector" in {
    val sqrtOf3Over3 = math.sqrt(3) / 3
    val n = Sphere.normalAt(Point(sqrtOf3Over3, sqrtOf3Over3, sqrtOf3Over3))
    n should be (n.normalize)
  }

  "The normal of a translated sphere" should "be translated in the world space" in {
    val s = Sphere.transform(translation(0, 1, 0))
    val n = s.normalAt(Point(0, 1.70711, -0.70711))
    n.map(roundTo5) should be (Vec(0, 0.70711, -0.70711))
  }

  "The normal on a transformed sphere" should "be transformed accordingly in the world space" in {
    val s = Sphere.transform(scaling(1, 0.5, 1) * rotationZ(math.Pi / 5))
    val n = s.normalAt(Point(0, math.sqrt(2) / 2, -math.sqrt(2) / 2))
    n.map(roundTo5) should be (Vec(0, 0.97014, -0.24254))
  }

  "A sphere" should "have the default material by default" in {
    Sphere.material should be (Material())
  }

  "A sphere" should "support updating of material" in {
    val newMat = Material().copy(ambient = 1)
    Sphere.withMaterial(newMat).material should be (newMat)
  }

  "A sphere with glassy material" should "have the correct transparency and refractive index" in {
    val glass = Sphere.Glass
    glass.transformation should be (Matrix.Identity4x4)
    glass.material.transparency should be (1.0)
    glass.material.refractiveIndex should be (1.5)
  }
}
