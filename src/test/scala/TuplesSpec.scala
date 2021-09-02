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
    Point(4, -4, 3) should be (Tuple(4, -4, 3, 1))
  }

  "vector()" should "create tuple with w=0.0"  in {
    Vec(4, -4, 3) should be (Tuple(4, -4, 3, 0))
  }

  "Adding two tuples" should "add all their components" in {
    val a1 = Tuple(3, -2, 5, 1)
    val a2 = Tuple(-2, 3, 1, 0)
    (a1 + a2) should be (Tuple(1, 1, 6, 1))
  }

  "Subtracting two points" should "subtract all their components" in {
    (Point(3, 2, 1) - Point(5, 6, 7)) should be (Vec(-2, -4, -6))
  }

  "Subtracting a vector from a point" should "yield another point" in {
    (Point(3, 2, 1) - Vec(5, 6, 7)) should be (Point(-2, -4, -6))
  }

  "Subtracting two vectors" should "yield another vector" in {
    val v1 = Vec(3, 2, 1)
    val v2 = Vec(5, 6, 7)
    (v1 - v2) should be (Vec(-2, -4, -6))
  }

  "Subtracting a vector from the zero vector" should "yield the opposite vector" in {
    (Vec.zero - Vec(1, -2, 3)) should be (Vec(-1, 2, -3))
  }

  "Negating a tuple" should "subtract it from the zero vector" in {
    val a = Tuple(1, -2, 3, -4)
    -a should be (Tuple(-1, 2, -3, 4))
  }

  "Multiplying a tuple by a scalar" should "multiply each component by the scalar" in {
    val a = Tuple(1, -2, 3, -4)
    (a * 3.5) should be (Tuple(3.5, -7, 10.5, -14))
  }

  "Multiplying a tuple by a fraction" should "result to division" in {
    val a = Tuple(1, -2, 3, -4)
    (a * 0.5) should be (Tuple(0.5, -1, 1.5, -2))
    (a * 0.5) should be (a / 2.0)
  }

  "Vector magnitude" should "yield the square root of the sum of the component's squares" in {
    Vec(1, 0, 0).magnitude should be (1)
    Vec(0, 1, 0).magnitude should be (1)
    Vec(0, 0, 1).magnitude should be (1)
    Vec(1, 2, 3).magnitude should be (math.sqrt(14))
    Vec(-1, -2, -3).magnitude should be (math.sqrt(14))
  }

  "Normalizing a vector" should "yield a unit vector" in {
    Vec(4, 0, 0).normalize should be (Vec(1, 0, 0))
    Vec(1, 2, 3).normalize should be (Vec(0.2672612419124244, 0.5345224838248488, 0.8017837257372732))
    Vec(1, 2, 3).normalize.magnitude should be (1)
  }

  "The dot product of 2 tuples" should "yield the sum of the products of their corresponding components" in {
    val a = Vec(1, 2, 3)
    val b = Vec(2, 3, 4)
    a.dot(b) should be (20)
  }
}
