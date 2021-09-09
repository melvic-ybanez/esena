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
}
