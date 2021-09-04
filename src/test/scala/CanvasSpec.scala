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

  it should "be convertible to PPM" in {
    val c = Canvas(5, 3)
    c.ppm.header should be (Vector(
      "P3",
      "5 3",
      "255"
    ))
  }

  it should "construct the PPM pixel data at the specified position" in {
    val c1 = Color(1.5, 0, 0)
    val c2 = Color(0, 0.5, 0)
    val c3 = Color(-0.5, 0, 1)
    val c = Canvas(5, 3).writePixel(0, 0, c1).writePixel(2, 1, c2).writePixel(4, 2, c3)
    c.ppm.pixelData should be (Vector(
      "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
      "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0",
      "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255",
    ))
  }
}
