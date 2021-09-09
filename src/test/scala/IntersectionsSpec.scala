import com.melvic.esena.rays.Intersection
import com.melvic.esena.shapes.Sphere
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class IntersectionsSpec extends AnyFlatSpec with should.Matchers {
  val s = new Sphere()
  val i = Intersection(3.5, s)
  i.t should be (3.5)
  i.obj should be (s)
}
