import com.melvic.esena.{Point, Tuple, Vec}
import org.scalatest._
import flatspec._
import matchers._

class TuplesSpec extends AnyFlatSpec with should.Matchers {
  "A point" should "have w=1.0" in {
    val a = Tuple(4.3, -4.2, 3.1, 1.0)
    a.x should be (4.3)
    a.y should be (-4.2)
    a.z should be (3.1)
    a.w should be (1.0)
    assert(Tuple.isPoint(a))
    assert(!Tuple.isVector(a))
  }

  "A vector" should "have w=0.0" in {
    val a = Tuple(4.3, -4.2, 3.1, 0.0)
    a.x should be (4.3)
    a.y should be (-4.2)
    a.z should be (3.1)
    a.w should be (0.0)
    assert(!Tuple.isPoint(a))
    assert(Tuple.isVector(a))
  }

  "point()" should "create tuple with w=1.0" in {
    val p = Point(4, -4, 3)
    p should be (Tuple(4, -4, 3, 1))
  }

  "vector()" should "create tuple with w=0.0"  in {
    val v = Vec(4, -4, 3)
    v should be (Tuple(4, -4, 3, 0))
  }

  "Adding two tuples" should "add all their components" in {
    val a1 = Tuple(3, -2, 5, 1)
    val a2 = Tuple(-2, 3, 1, 0)
    (a1 + a2) should be (Tuple(1, 1, 6, 1))
  }

  "Subtracting two points" should "subtract all their components" in {
    val p1 = Point(3, 2, 1)
    val p2 = Point(5, 6, 7)
    (p1 - p2) should be (Vec(-2, -4, -6))
  }

  "Subtracting a vector from a point" should "yield another point" in {
    val p = Point(3, 2, 1)
    val v = Vec(5, 6, 7)
    (p - v) should be (Point(-2, -4, -6))
  }

  "Subtracting two vectors" should "yield another vector" in {
    val v1 = Vec(3, 2, 1)
    val v2 = Vec(5, 6, 7)
    (v1 - v2) should be (Vec(-2, -4, -6))
  }

  "Subtracting a vector from the zero vector" should "yield the opposite vector" in {
    val v = Vec(1, -2, 3)
    (Vec.zero - v) should be (Vec(-1, 2, -3))
  }

  "Negating a tuple" should "subtract it from the zero vector" in {
    val a = Tuple(1, -2, 3, -4)
    -a should be (Tuple(-1, 2, -3, 4))
  }
}
