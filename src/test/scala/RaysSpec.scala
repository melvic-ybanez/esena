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
}
