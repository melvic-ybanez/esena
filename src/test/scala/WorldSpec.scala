import com.melvic.esena.MathUtils.{roundTo, roundTo5}
import com.melvic.esena.canvas.Color
import com.melvic.esena.{MathUtils, lights, dielectrics}
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.{scaling, translation}
import com.melvic.esena.patterns.TestPattern
import com.melvic.esena.rays.{Computations, Intersection, Intersections, Ray}
import com.melvic.esena.dielectrics.Refraction.index.Glass
import com.melvic.esena.scene.World
import com.melvic.esena.shapes.{Plane, Sphere}
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class WorldSpec extends AnyFlatSpec with should.Matchers {
  val world = World.default

  "An empty world" should "contain no objects and no light source" in {
    val w = World()
    w.light should be (None)
    w.objects should be (Vector())
  }

  "The default world" should "have the default settings for light and objects" in {
    val light = PointLight(Point(-10, 10, -10), Color.White)
    val s1 = Sphere.withMaterial(
      Material(
        color = Color(0.8, 1.0, 0.6),
        diffuse = 0.7,
        specular = 0.2
      )
    )
    val s2 = Sphere.transform(scaling(0.5, 0.5, 0.5))
    world.light should be (Some(light))
    assert(world.contains(s1))
    assert(world.contains(s2))
  }

  "Intersecting a world with a ray" should "return all intersections with the world's objects" in {
    val xs = world.intersect(Ray(Point(0, 0, -5), Vec(0, 0, 1)))
    xs.size should be (4)
    xs(0).t should be (4)
    xs(1).t should be (4.5)
    xs(2).t should be (5.5)
    xs(3).t should be (6)
  }

  "Shading an intersection" should "return the color from the given world" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val shape = world.objects.head
    val i = Intersection(4, shape)
    val comps = Computations.prepare(i, r)
    val c = lights.shadeHit(world, comps)
    c.map(roundTo5) should be (Color(0.38066, 0.47583, 0.28550))
  }

  "Shading an intersection" should "be possible from the inside" in {
    val w = World.default.withLight(PointLight(Point(0, 0.25, 0), Color.White))
    val r = Ray(Point.Origin,  Vec(0, 0, 1))
    val shape = w.objects(1)
    val i = Intersection(0.5, shape)
    val comps = Computations.prepare(i, r)
    lights.shadeHit(w, comps) should be (Color(0.1, 0.1, 0.1))
  }

  "The color when a ray misses" should "be black" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 1, 0))
    world.colorAt(r) should be (Color.Black)
  }

  "The color when a ray hits" should "be from the object the ray intersects with" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val c = world.colorAt(r)
    c.map(roundTo5) should be (Color(0.38066, 0.47583, 0.28550))
  }

  "The color with an intersection behind the ray" should "return the color of the pointed-to object" in {
    val outer = world.objects.head match {
      case s: Sphere => s.withMaterial(s.material.copy(ambient = 1))
    }
    val inner = world.objects(1) match {
      case s: Sphere => s.withMaterial(s.material.copy(ambient = 1))
    }
    val updatedWorld = world.copy(objects = Vector(outer, inner))
    val r = Ray(Point(0, 0, 0.75), Vec(0, 0, -1))
    updatedWorld.colorAt(r) should be (inner.material.color)
  }

  "Shadow" should "not be produced when nothing lies between the point and the light source" in {
    val point = Point(0, 10, 0)
    assert(!world.isShadowedAt(point))
  }

  "An object between the point and the light source" should "cause a shadow" in {
    val point = Point(10, -10, 10)
    assert(world.isShadowedAt(point))
  }

  "An object behind the light" should "not cause a shadow" in {
    val point = Point(-20, 20, -20)
    assert(!world.isShadowedAt(point))
  }

  "An object behind the point" should "not cause a shadow" in {
    val point = Point(-2, 2, -2)
    assert(!world.isShadowedAt(point))
  }

  "If an intersection is in shadow, the object" should "only have ambient color" in {
    val s1 = Sphere
    val s2 = Sphere.transform(translation(0, 0, 10))
    val world = World
      .default
      .withLight(PointLight(Point(0, 0, -10), Color.White))
      .addObjects(s1, s2)
    val ray = Ray(Point(0, 0, 5), Vec(0, 0, 1))
    val intersection = Intersection(4, s2)
    val comps = Computations.prepare(intersection, ray)
    val color = lights.shadeHit(world, comps)
    color should be (Color(0.1, 0.1, 0.1))
  }

  "The reflected color for a non-reflective material" should "be black" in {
    val ray = Ray(Point.Origin, Vec(0, 0, 1))
    val shape = world.objects(1).updateMaterial(_.copy(ambient = 1))
    val i = Intersection(1, shape)
    val comps = Computations.prepare(i, ray)
    val color = dielectrics.reflectedColor(world, comps)
    color should be (Color.Black)
  }

  "The reflected color for a reflective material" should "reflect a darker color" in {
    // a plane below the spheres
    val shape = Plane.updateMaterial(_.copy(reflective = 0.5)).translate(0, -1, 0)

    val newWorld = world.addObjects(shape)

    // strike the plane to reflect upwards, hitting the outermost sphere
    val ray = Ray(Point(0, 0, -3), Vec(0, -math.sqrt(2) / 2, math.sqrt(2) / 2))

    val i = Intersection(math.sqrt(2), shape)
    val comps = Computations.prepare(i, ray)
    val color = dielectrics.reflectedColor(newWorld, comps)
    color.map(roundTo5) should be (Color(0.19033, 0.23791, 0.14275))
  }

  "Shade Hit" should "consider reflective material" in {
    val shape = Plane.updateMaterial(_.copy(reflective = 0.5)).translate(0, -1, 0)
    val newWorld = world.addObjects(shape)
    val ray = Ray(Point(0, 0, -3), Vec(0, -math.sqrt(2) / 2, math.sqrt(2) / 2))
    val i = Intersection(math.sqrt(2), shape)
    val comps = Computations.prepare(i, ray)
    val color = lights.shadeHit(newWorld, comps)
    color.map(roundTo5) should be (Color(0.87676, 0.92434, 0.82917))
  }

  "colorAt with mutually reflective surfaces" should "not cause infinite recursion" in {
    val lower = Plane.updateMaterial(_.copy(reflective = 1)).translate(0, -1, 0)
    val upper = Plane.updateMaterial(_.copy(reflective = 1)).translate(0, 1, 0)
    val newWorld = world.withLight(PointLight(Point.Origin, Color.White)).addObjects(lower, upper)
    val ray = Ray(Point.Origin, Vec(0, 1, 0))

    // see if the program terminates
    newWorld.colorAt(ray) should be (Color(0.1, 0.1, 0.1))
  }

  "The reflected color at the maximum recursive depth" should "return the color immediately" in {
    val shape = Plane.updateMaterial(_.copy(reflective = 0.5)).translate(0, -1, 0)
    val newWorld = world.addObjects(shape)
    val ray = Ray(Point(0, 0, -3), Vec(0, -math.sqrt(2) / 2, math.sqrt(2) / 2))
    val i = Intersection(math.sqrt(2), shape)
    val comps = Computations.prepare(i, ray)
    val color = dielectrics.reflectedColor(newWorld, comps, 0)
    color should be (Color.Black)
  }

  "The refracted color of an opaque surface" should "be black" in {
    val shape = world.objects.head
    val ray = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val xs = Intersections.fromPairs(4 -> shape, 6 -> shape)
    val comps = Computations.prepare(xs.head, ray, xs)
    val color = dielectrics.refractedColor(world, comps, 5)
    color should be (Color.Black)
  }

  "The refracted color at the maximum recursive depth" should "be black" in {
    val shape = world.objects.head.updateMaterial(_.copy(transparency = 1.0, refractiveIndex = 1.5))
    val ray = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val xs = Intersections.fromPairs(4 -> shape, 6 -> shape)
    val comps = Computations.prepare(xs.head, ray, xs)
    val color = dielectrics.refractedColor(world.updateObject(0, shape), comps, 0)
    color should be (Color.Black)
  }

  "The refracted color under total internal reflection" should "be black" in {
    val shape = world.objects.head.updateMaterial(_.copy(transparency = 1.0, refractiveIndex = 1.5))
    val ray = Ray(Point(0, 0, math.sqrt(2) / 2), Vec(0, 1, 0))
    val xs = Intersections.fromPairs(-math.sqrt(2) /  2 -> shape, math.sqrt(2) / 2 -> shape)

    // check the second intersection instead of the first because
    // we are inside the sphere
    val comps = Computations.prepare(xs(1), ray, xs)
    val color = dielectrics.refractedColor(world.updateObject(0, shape), comps, 5)
    color should be (Color.Black)
  }

  "The refracted ray" should "return the correct refracted color" in {
    val a = world.objects(0).updateMaterial(_.copy(ambient = 1.0, pattern = Some(TestPattern())))
    val b = world.objects(1).updateMaterial(_.copy(transparency = 1.0, refractiveIndex = 1.5))
    val ray = Ray(Point(0, 0, 0.1), Vec(0, 1, 0))
    val xs = Intersections.fromPairs(-0.9899 -> a, -0.4899 -> b, 0.4899 -> b, 0.9899 -> a)
    val comps = Computations.prepare(xs(2), ray, xs)
    val color = dielectrics.refractedColor(world.updateObject(0, a).updateObject(1, b), comps, 5)
    color.map(roundTo5) should be (Color(0, 0.99889, 0.04722))
  }

  "Shade-hit" should "support transparent material" in {
    val floor = Plane.translate(0, -1, 0).updateMaterial(_.copy(transparency = 0.5, refractiveIndex = 1.5))
    val ball = Sphere.updateMaterial(_.copy(color = Color(1, 0, 0), ambient = 0.5)).translate(0, -3.5, -0.5)
    val newWorld = world.addObjects(floor, ball)
    val ray = Ray(Point(0, 0, -3), Vec(0, -math.sqrt(2) / 2, math.sqrt(2) / 2))
    val xs = Intersections.fromPairs(math.sqrt(2) -> floor)
    val comps = Computations.prepare(xs(0), ray, xs)
    val color = lights.shadeHit(newWorld, comps, 5)
    color.map(roundTo5) should be (Color(0.93643, 0.68643, 0.68643))
  }

  "Shade-hit with a reflective, transparent material" should "include reflected and refracted colors" in {
    val ray = Ray(Point(0, 0, -3), Vec(0, -math.sqrt(2) / 2, math.sqrt(2) / 2))
    val floor = Plane
      .translate(0, -1, 0)
      .withMaterial(Material(reflective = 0.5, transparency = 0.5, refractiveIndex = 1.5))
    val ball = Sphere
      .withMaterial(Material(color = Color(1, 0, 0), ambient = 0.5))
      .translate(0, -3.5, -0.5)
    val newWorld = world.addObjects(floor, ball)
    val xs = Intersections(math.sqrt(2) -> floor)
    val comps = Computations.prepare(xs(0), ray, xs)
    val color = lights.shadeHit(newWorld, comps, 5)
    color.map(roundTo5) should be (Color(0.93392, 0.69643, 0.69243))
  }
}
