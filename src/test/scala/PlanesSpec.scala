import com.melvic.esena.shapes.Plane
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PlanesSpec extends AnyFlatSpec with should.Matchers {
  "The normal of a plane" should "be constant everywhere" in {
    val plane = Plane()

    val n1 = plane.localNormalAt(Point.Origin)
    val n2 = plane.localNormalAt(Point(10, 0, -10))
    val n3 = plane.localNormalAt(Point(-5, 0, 150))

    n1 should be (Vec(0, 1, 0))
    n2 should be (Vec(0, 1, 0))
    n3 should be (Vec(0, 1, 0))
  }
}
