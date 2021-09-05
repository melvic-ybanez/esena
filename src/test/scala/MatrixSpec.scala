import com.melvic.esena.Tuple
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

  it should "be equal to a matrix with similar elements" in {
    val a = Matrix.fromRows(
      Vector(1, 2, 3, 4),
      Vector(5, 6, 7, 8),
      Vector(9, 8, 7, 6),
      Vector(5, 4, 3, 2)
    )
    val b = Matrix.fromRows(
      Vector(1, 2, 3, 4),
      Vector(5, 6, 7, 8),
      Vector(9, 8, 7, 6),
      Vector(5, 4, 3, 2)
    )
    a should be (b)
  }

  it should "not be equal to a matrix with different elements" in {
    val a = Matrix.fromRows(
      Vector(1, 2, 3, 4),
      Vector(5, 6, 7, 8),
      Vector(9, 8, 7, 6),
      Vector(5, 4, 3, 2)
    )
    val b = Matrix.fromRows(
      Vector(2, 3, 4, 5),
      Vector(6, 7, 8, 9),
      Vector(8, 7, 6, 5),
      Vector(4, 3, 2, 1)
    )
    a should not be b
  }

  it should "support matrix multiplication" in {
    val a = Matrix.fromRows(
      Vector(1, 2, 3, 4),
      Vector(5, 6, 7, 8),
      Vector(9, 8, 7, 6),
      Vector(5, 4, 3, 2)
    )
    val b = Matrix.fromRows(
      Vector(-2, 1, 2, 3),
      Vector(3, 2, 1, -1),
      Vector(4, 3, 6, 5),
      Vector(1, 2, 7, 8)
    )
    (a * b) should be (Matrix.fromRows(
      Vector(20, 22, 50, 48),
      Vector(44, 54, 114, 108),
      Vector(40, 58, 110, 102),
      Vector(16, 26, 46, 42)
    ))
  }

  it should "be constructable from a tuple" in {
    Matrix.fromTuple(Tuple(1, 2, 3, 1)) should be (Matrix.fromRows(
      Vector(1), Vector(2), Vector(3), Vector(1)
    ))
  }

  it should "support matrix-tuple multiplication" in {
    val a = Matrix.fromRows(
      Vector(1, 2, 3, 4),
      Vector(2, 4, 4, 2),
      Vector(8, 6, 4, 1),
      Vector(0, 0, 0, 1)
    )
    val b = Tuple(1, 2, 3, 1)
    (a * b) should be (Tuple(18, 24, 33, 1))
  }

  "The identity matrix multiplied by any other matrix m" should "yield m" in {
    val a = Matrix.fromRows(
      Vector(0, 1, 2, 4),
      Vector(1, 2, 4, 8),
      Vector(2, 4, 8, 6),
      Vector(4, 8, 16, 32)
    )
    (a * Matrix.identity(4, 4)) should be (a)
  }

  "Transposing a matrix" should "switch the rows and columns" in {
    val a = Matrix.fromRows(
      Vector(0, 9, 3, 0),
      Vector(9, 8, 0, 8),
      Vector(1, 8, 5, 3),
      Vector(0, 0, 5, 8)
    )
    a.transpose should be (Matrix.fromRows(
      Vector(0, 9, 1, 0),
      Vector(9, 8, 8, 0),
      Vector(3, 0, 5, 5),
      Vector(0, 8, 3, 8)
    ))
  }
}
