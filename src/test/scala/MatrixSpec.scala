import com.melvic.esena.matrix.Matrix
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MatrixSpec extends AnyFlatSpec with should.Matchers {
  "A matrix" should "be provide access to an element by position" in {
    val m = Matrix.fromRows(
      Vector(1, 2, 3, 4),
      Vector(5.5, 6.5, 7.5, 8.5),
      Vector(9, 10, 11, 12),
      Vector(13.5, 14.5, 15.5, 16.5)
    )
    m(0, 0) should be (1)
    m(0, 3) should be (4)
    m(1, 0) should be (5.5)
    m(1, 2) should be (7.5)
    m(2, 2) should be (11)
    m(3, 0) should be (13.5)
    m(3, 2) should be (15.5)
  }
}
