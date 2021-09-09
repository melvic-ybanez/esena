import com.melvic.esena.rays.Intersection
import com.melvic.esena.shapes.Sphere
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class IntersectionsSpec extends AnyFlatSpec with should.Matchers {
  "An intersection" should "encapsulates t and object" in {
    val s = new Sphere()
    val i = Intersection(3.5, s)
    i.t should be (3.5)
    i.obj should be (s)
  }

  "Integrations" should "be able to aggregate" in {
    val s = new Sphere
    val i1 = Intersection(1, s)
    val i2 = Intersection(2, s)
    val xs = Intersection.aggregate(i1, i2)
    xs.size should be (2)
    xs(0).t should be (1)
    xs(1).t should be (2)
  }
}
