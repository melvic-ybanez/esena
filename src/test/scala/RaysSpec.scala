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
}
