import com.melvic.esena.{Canvas, Color}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CanvasSpec extends AnyFlatSpec with should.Matchers {
  "Canvas size" should "be configurable" in {
    val c = Canvas(10, 20)
    c.width should be (10)
    c.height should be (20)
    c.pixels.foreach(_ should be (Color.Black))
  }

  it should "support updating by pixel position" in {
    val c = Canvas(10, 20)
    val c1 = c.writePixel(2, 3, Color.Red)
    c1.pixelAt(2, 3) should be (Color.Red)
  }
}
