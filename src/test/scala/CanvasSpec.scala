import com.melvic.esena.{Canvas, Color}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CanvasSpec extends AnyFlatSpec with should.Matchers {
  "Canvas size" should "be configurable" in {
    val c = Canvas(10, 20)
    c.width should be (10)
    c.height should be (20)
    c.pixels.foreach(_ should be (Color(0, 0, 0)))
  }
}
