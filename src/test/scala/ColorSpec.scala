import com.melvic.esena.canvas.Color
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ColorSpec extends AnyFlatSpec with should.Matchers {
  "Colors" should "be represented as RGB tuples" in {
    val c = Color(-0.5, 0.4, 1.7)
    c.red should be (-0.5)
    c.green should be (0.4)
    c.blue should be (1.7)
  }

  "Adding colors" should "be similar to adding tuples" in {
    Color(0.9, 0.6, 0.75) + Color(0.7, 0.1, 0.25) should be (Color(1.6, 0.7, 1.0))
  }

  "Subtracting colors" should "be similar to subtracting tuples" in {
    Color(0.9, 0.6, 0.75) - Color(0.7, 0.1, 0.25) should be (Color(0.2, 0.5, 0.5))
  }

  "Multiplying a color by a scalar" should "be similar to multiplying a tuple by a scalar" in {
    Color(0.2, 0.3, 0.4) * 2.0 should be (Color(0.4, 0.6, 0.8))
  }

  "Multiplying colors" should "compute their hadamard products" in {
    Color(1, 0.2, 0.4) * Color(0.9, 1, 0.1) should be (Color(0.9, 0.2, 0.04))
  }
}
