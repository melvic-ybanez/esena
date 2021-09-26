import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Plane
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PlanesSpec extends AnyFlatSpec with should.Matchers {
  val plane = Plane

  "The normal of a plane" should "be constant everywhere" in {
    val n1 = plane.localNormalAt(Point.Origin)
    val n2 = plane.localNormalAt(Point(10, 0, -10))
    val n3 = plane.localNormalAt(Point(-5, 0, 150))

    n1 should be (Vec(0, 1, 0))
    n2 should be (Vec(0, 1, 0))
    n3 should be (Vec(0, 1, 0))
  }

  "A ray parallel to the plane" should "miss the plane" in {
    val ray = Ray(Point(0, 10, 0), Vec(0, 0, 1))
    val xs = plane.localIntersect(ray)
    xs should be (Vector())
  }

  "A ray coplanar with the plane" should "miss the plane" in {
    val ray = Ray(Point.Origin, Vec(0, 0, 1))
    val xs = plane.localIntersect(ray)
    xs should be (Vector())
  }

  "A ray intersecting a plane from above" should "compute the t as -origin/direction" in {
    val ray = Ray(Point(0, 1, 0), Vec(0, -1, 0))
    val xs = plane.localIntersect(ray)
    xs.size should be (1)
    xs.head.t should be (1)
    xs.head.obj should be (plane)
  }

  "A ray intersecting a plane from below" should "compute the t as -origin/direction" in {
    val ray = Ray(Point(0, -1, 0), Vec(0, 1, 0))
    val xs = plane.localIntersect(ray)
    xs.size should be (1)
    xs.head.t should be (1)
    xs.head.obj should be (plane)
  }
}
