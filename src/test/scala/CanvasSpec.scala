import com.melvic.esena.Color
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CanvasSpec extends AnyFlatSpec with should.Matchers {
  "Colors" should "be represented as RGB tuples" in {
    val c = Color(-0.5, 0.4, 1.7)
    c.red should be (-0.5)
    c.green should be (0.4)
    c.blue should be (1.7)
  }
}
