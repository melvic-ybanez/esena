import com.melvic.esena.matrix.Matrix4D
import com.melvic.esena.matrix.Matrix4D.{scaling, translation}
import com.melvic.esena.rays.Ray
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RaysSpec extends AnyFlatSpec with should.Matchers {
  "A ray" should "have an origin and direction" in {
    val origin = Point(1, 2, 3)
    val direction = Vec(4, 5, 6)
    val ray = Ray(origin, direction)
    ray.origin should be (origin)
    ray.direction should be (direction)
  }

  it should "be able to compute a point from a distance" in {
    val r = Ray(Point(2, 3, 4), Vec(1, 0, 0))
    r(0) should be (Point(2, 3, 4))
    r(1) should be (Point(3, 3, 4))
    r(-1) should be (Point(1, 3, 4))
    r(2.5) should be (Point(4.5, 3, 4))
  }

  "Translating a ray" should "map a transformation matrix to a new ray" in {
    val r = Ray(Point(1, 2, 3), Vec(0, 1, 0))
    val r2 = r.transform(translation(3, 4, 5))
    r2.origin should be (Point(4, 6, 8))
    r2.direction should be (Vec(0, 1, 0))
  }

  "Scaling a ray" should "map a scaling matrix to a new ray" in {
    val r = Ray(Point(1, 2, 3), Vec(0, 1, 0))
    val r2 = r.transform(scaling(2, 3, 4))
    r2.origin should be (Point(2, 6, 12))
    r2.direction should be (Vec(0, 3, 0))
  }
}
