import com.melvic.esena.matrix.Matrix
import com.melvic.esena.tuples.Tuple
import com.melvic.esena.Math._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MatrixSpec extends AnyFlatSpec with should.Matchers {
  "A matrix" should "be provide access to an element by position" in {
    val m = Matrix.of4x4(
      (1, 2, 3, 4),
      (5.5, 6.5, 7.5, 8.5),
      (9, 10, 11, 12),
      (13.5, 14.5, 15.5, 16.5)
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
    val a = Matrix.of4x4(
      (1, 2, 3, 4),
      (5, 6, 7, 8),
      (9, 8, 7, 6),
      (5, 4, 3, 2)
    )
    val b = Matrix.of4x4(
      (1, 2, 3, 4),
      (5, 6, 7, 8),
      (9, 8, 7, 6),
      (5, 4, 3, 2)
    )
    a should be (b)
  }

  it should "not be equal to a matrix with different elements" in {
    val a = Matrix.of4x4(
      (1, 2, 3, 4),
      (5, 6, 7, 8),
      (9, 8, 7, 6),
      (5, 4, 3, 2)
    )
    val b = Matrix.of4x4(
      (2, 3, 4, 5),
      (6, 7, 8, 9),
      (8, 7, 6, 5),
      (4, 3, 2, 1)
    )
    a should not be b
  }

  it should "support matrix multiplication" in {
    val a = Matrix.of4x4(
      (1, 2, 3, 4),
      (5, 6, 7, 8),
      (9, 8, 7, 6),
      (5, 4, 3, 2)
    )
    val b = Matrix.of4x4(
      (-2, 1, 2, 3),
      (3, 2, 1, -1),
      (4, 3, 6, 5),
      (1, 2, 7, 8)
    )
    (a * b) should be (Matrix.of4x4(
      (20, 22, 50, 48),
      (44, 54, 114, 108),
      (40, 58, 110, 102),
      (16, 26, 46, 42)
    ))
  }

  it should "be constructable from a tuple" in {
    Matrix.fromTuple(Tuple(1, 2, 3, 1)) should be (Matrix.fromRows(
      Vector(1), Vector(2), Vector(3), Vector(1)
    ))
  }

  it should "support matrix-tuple multiplication" in {
    val a = Matrix.of4x4(
      (1, 2, 3, 4),
      (2, 4, 4, 2),
      (8, 6, 4, 1),
      (0, 0, 0, 1)
    )
    val b = Tuple(1, 2, 3, 1)
    (a * b) should be (Tuple(18, 24, 33, 1))
  }

  "The identity matrix multiplied by any other matrix m" should "yield m" in {
    val a = Matrix.of4x4(
      (0, 1, 2, 4),
      (1, 2, 4, 8),
      (2, 4, 8, 6),
      (4, 8, 16, 32)
    )
    (a * Matrix.identity(4, 4)) should be (a)
  }

  "Transposing a matrix" should "switch the rows and columns" in {
    val a = Matrix.of4x4(
      (0, 9, 3, 0),
      (9, 8, 0, 8),
      (1, 8, 5, 3),
      (0, 0, 5, 8)
    )
    a.transpose should be (Matrix.of4x4(
      (0, 9, 1, 0),
      (9, 8, 8, 0),
      (3, 0, 5, 5),
      (0, 8, 3, 8)
    ))
  }

  "Transposing the identity matrix" should "yield the identity matrix" in {
    Matrix.Identity4x4.transpose should be (Matrix.Identity4x4)
  }

  "The determinant of 2x2 matrix" should "be ad - bc where a, b, c, and d the elements" in {
    val m = Matrix.fromRows(
      Vector(1, 5),
      Vector(-3, 2)
    )
    m.determinant should be (17)
  }

  "A Matrix" should "provide submatrices based on the given row and column" in {
    val _3by3 = Matrix.of3x3(
      (1, 5, 0),
      (-3, 2, 7),
      (0, 6, -3),
    )
    _3by3.subMatrix(0, 2) should be (Matrix.fromRows(
      Vector(-3, 2), Vector(0, 6)
    ))

    val _4by4 = Matrix.of4x4(
      (-6, 1, 1, 6),
      (-8, 5, 8, 6),
      (-1, 0, 8, 2),
      (-7, 1, -1, 1)
    )
    _4by4.subMatrix(2, 1) should be (Matrix.of3x3(
      (-6, 1, 6),
      (-8, 8, 6),
      (-7, -1, 1)
    ))
  }

  "A minor" should "be the determinant of the submatrix(i, j) for some i and j" in {
    val m = Matrix.of3x3(
      (3, 5, 0),
      (2, -1, -7),
      (6, -1, 5)
    )
    val b = m.subMatrix(1, 0)
    b.determinant should be (25)
    m.minor(1, 0) should be (25)
  }

  "A cofactor" should "return the minor with the signs possibly reversed" in {
    val a = Matrix.of3x3(
      (3, 5, 0),
      (2, -1, -7),
      (6, -1, 5)
    )
    a.minor(0, 0) should be (-12)
    a.cofactor(0, 0) should be (-12)
    a.minor(1, 0) should be (25)
    a.cofactor(1, 0) should be (-25)
  }

  "Computing the determinant" should "use the generic formula for 3x3 matrix" in {
    val m = Matrix.of3x3(
      (1, 2, 6),
      (-5, 8, -4),
      (2, 6, 4),
    )
    m.cofactor(0, 0) should be (56)
    m.cofactor(0, 1) should be (12)
    m.cofactor(0, 2) should be (-46)
    m.determinant should be (-196)
  }

  it should "also be possible for 4x4 matrix" in {
    val m = Matrix.of4x4(
      (-2, -8, 3, 5),
      (-3, 1, 7, 3),
      (1, 2, -9, 6),
      (-6, 7, 7, -9)
    )
    m.cofactor(0, 0) should be (690)
    m.cofactor(0, 1) should be (447)
    m.cofactor(0, 2) should be (210)
    m.cofactor(0, 3) should be (51)
    m.determinant should be (-4071)
  }

  "A matrix with zero determinant" should "be invertible" in {
    val a = Matrix.of4x4(
      (6, 4, 4, 4),
      (5, 5, 7, 6),
      (4, -9, 3, -7),
      (9, 1, 7, -6)
    )
    a.determinant should be (-2120)
    assert(a.isInvertible)
  }

  "A matrix with non-zero determinant" should "not be invertible" in {
    val a = Matrix.of4x4(
      (-4, 2, -2, 3),
      (9, 6, 2, 6),
      (0, -5, 1, -5),
      (0, 0, 0, 0)
    )
    a.determinant should be (0)
    assert(!a.isInvertible)
  }

  "An invertible matrix" should "calculate the correct inverse" in {
    val a = Matrix.of4x4(
      (-5, 2, 6, -8),
      (1, -5, 1, 8),
      (7, 7, -6, -7),
      (1, -3, 7, 4)
    )

    val ai = a.inverse
    a.determinant should be (532)
    a.cofactor(2, 3) should be (-160)
    ai(3, 2) should be (-160.0 / 532)
    a.cofactor(3, 2) should be (105)
    ai(2, 3) should be (105.0 / 532)
    ai.map(roundTo5) should be (Matrix.of4x4(
      (0.21805, 0.45113, 0.24060, -0.04511),
      (-0.80827, -1.45677, -0.44361, 0.52068),
      (-0.07895, -0.22368, -0.05263, 0.19737),
      (-0.52256, -0.81391, -0.30075, 0.30639)
    ))

    val b = Matrix.of4x4(
      (8, -5, 9, 2),
      (7, 5, 6, 1),
      (-6, 0, 9, 6),
      (-3, 0, -9, -4)
    )
    b.inverse.map(roundTo5) should be (Matrix.of4x4(
      (-0.15385, -0.15385, -0.28205, -0.53846),
      (-0.07692, 0.12308, 0.02564, 0.03077),
      (0.35897, 0.35897, 0.43590, 0.92308),
      (-0.69231, -0.69231, -0.76923, -1.92308)
    ))

    val c = Matrix.of4x4(
      (9, 3, 0, 9),
      (-5, -2, -6, -3),
      (-4, 9, 6, 4),
      (-7, 6, 6, 2)
    )
    c.inverse.map(roundTo5) should be (Matrix.of4x4(
      (-0.04074, -0.07778, 0.14444, -0.22222),
      (-0.07778, 0.03333, 0.36667, -0.33333),
      (-0.02901, -0.14630, -0.10926, 0.12963),
      (0.17778, 0.06667, -0.26667, 0.33333)
    ))
  }

  "Multiplying a A * B by the inverse of A" should "return B" in {
    val a = Matrix.of4x4(
      (3, -9, 7, 3),
      (3, -8, 2, -9),
      (-4, 4, 4, 1),
      (-6, 5, -1, 1)
    )
    val b = Matrix.of4x4(
      (8, 2, 2, 2),
      (3, -1, 7, 0),
      (7, 0, 5, 4),
      (6, -2, 0, 5)
    )
    val c = a * b
    (c * b.inverse) should be (a)
  }
}
