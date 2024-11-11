package ch8

import munit.FunSuite

class HelloSpec extends FunSuite {
/*
  test("&& should return true when both properties pass") {
    val prop1 = new Prop { def check: Boolean = true }
    val prop2 = new Prop { def check: Boolean = true }

    val combinedProp = prop1 && prop2
    assert(combinedProp.check == true)
  }

  test("&& should return false when the first property fails") {
    val prop1 = new Prop { def check: Boolean = false }
    val prop2 = new Prop { def check: Boolean = true }

    val combinedProp = prop1 && prop2
    assert(combinedProp.check == false)
  }

  test("&& should return false when the second property fails") {
    val prop1 = new Prop { def check: Boolean = true }
    val prop2 = new Prop { def check: Boolean = false }

    val combinedProp = prop1 && prop2
    assert(combinedProp.check == false)
  }

  test("&& should return false when both properties fail") {
    val prop1 = new Prop { def check: Boolean = false }
    val prop2 = new Prop { def check: Boolean = false }

    val combinedProp = prop1 && prop2
    assert(combinedProp.check == false)
  }
*/

  test("Gen.choose should generate numbers within the specified range") {
    val rng = SimpleRNG(42)
    val gen = Gen.choose(10, 20)

    val (n, _) = gen.sample(rng)
    assert(n >= 10 && n < 20, s"Generated number $n is not within the range [10, 20)")
  }

  test("Gen.choose should throw an error for invalid range") {
    intercept[IllegalArgumentException] {
      Gen.choose(20, 10)
    }
  }

  test("Gen.choose should produce a different number with a different RNG seed") {
    val rng1 = SimpleRNG(43)
    val rng2 = SimpleRNG(100)
    val gen = Gen.choose(1, 100)

    val (n1, _) = gen.sample(rng1)
    val (n2, _) = gen.sample(rng2)

    // ig this depends on the range of values, but whatever.
    assertNotEquals(n1, n2, s"Generated numbers $n1 and $n2 should (probably) not be the same with different seeds")
  }

  test("Gen.choose should produce the same number with the same RNG seed") {
    val rng = SimpleRNG(42)
    val gen = Gen.choose(1, 10)

    val (n1, _) = gen.sample(rng)
    val (n2, _) = gen.sample(rng)

    assertEquals(n1, n2, s"Generated numbers $n1 and $n2 should be the same with the same seed")
  }

  test("Gen.unit should always produce the same value") {
    val gen = Gen.unit(42)
    val rng = SimpleRNG(1)
    val (result, _) = gen.sample(rng)
    assert(result == 42)
  }

  test("Gen.unit should not change the RNG state") {
    val gen = Gen.unit("constant")
    val rng = SimpleRNG(1)
    val (_, nextRng) = gen.sample(rng)
    assert(rng == nextRng)
  }

  test("Gen.boolean should produce true or false") {
    val gen = Gen.boolean
    val rng = SimpleRNG(42)
    val (result, _) = gen.sample(rng)
    assert(result == true || result == false, s"Expected true or false, got $result")
  }


  test("Gen.boolean should produce a mix of true and false") {
    val gen = Gen.boolean
    var rng: RNG = SimpleRNG(1)
    
    val results = (1 to 100).map { _ =>
      val (value, nextRng) = gen.sample(rng)
      rng = nextRng
      value
    }

    assert(results.contains(true) && results.contains(false), "Expected both true and false values")
  }

  test("Gen.listOfN should produce a list of the specified length") {
    val gen = Gen.listOfN(5, Gen.unit(42))
    val rng = SimpleRNG(1)
    val (result, _) = gen.sample(rng)
    assertEquals(result.length, 5)
  }

  test("Gen.listOfN should produce a list with elements from the generator") {
    val gen = Gen.listOfN(3, Gen.unit("Scala"))
    val rng = SimpleRNG(1)
    val (result, _) = gen.sample(rng)
    assert(result.forall(_ == "Scala"))
  }

  test("Gen.listOfN should produce different lists with different RNG states") {
    val gen = Gen.listOfN(3, Gen.choose(1, 10))
    val rng1 = SimpleRNG(42)
    val rng2 = SimpleRNG(100)

    val (result1, _) = gen.sample(rng1)
    val (result2, _) = gen.sample(rng2)

    assertNotEquals(result1, result2)
  }


  test("Gen.flatMap should generate values based on the initial value") {
    val intGen = Gen.unit(5)
    val doubleGen = intGen.flatMap(i => Gen.unit(i * 2.0))  // Should generate 10.0

    val rng = SimpleRNG(42)
    val (result, _) = doubleGen.sample(rng)

    assertEquals(result, 10.0)
  }

  test("Gen.flatMap should produce different values with different seeds") {
    val intGen = Gen.choose(1, 10)
    val dependentGen = intGen.flatMap(i => Gen.unit(i * 2)) // Should double the chosen value

    val rng1 = SimpleRNG(1)
    val rng2 = SimpleRNG(2)

    val (result1, _) = dependentGen.sample(rng1)
    val (result2, _) = dependentGen.sample(rng2)

    assertNotEquals(result1, result2, s"Expected different results but got $result1 and $result2")
  }

  test("Gen.listOfN should generate lists of varying lengths") {
    val sizeGen = Gen.choose(1, 5)
    val listGen = Gen.unit(42).listOfN(sizeGen) // Lists of length 1 to 5, with elements as 42

    val rng = SimpleRNG(42)
    val (result, _) = listGen.sample(rng)

    assert(result.length >= 1 && result.length <= 5, s"Expected list length between 1 and 5, got ${result.length}")
    assert(result.forall(_ == 42), s"Expected all elements to be 42, but got $result")
  }

  test("Gen.union should generate values from both generators over many samples") {
    val g1 = Gen.unit(1)
    val g2 = Gen.unit(2)
    val unionGen = Gen.union(g1, g2)

    var rng: RNG = SimpleRNG(42)
    val results = (1 to 100).map { _ =>
      val (result, nextRng) = unionGen.sample(rng)
      rng = nextRng  // Update RNG state
      result
    }

    assert(results.contains(1) && results.contains(2), s"Expected results to contain both 1 and 2, but got $results")
  }

  test("Gen.union should produce approximately equal distribution") {
    val g1 = Gen.unit("A")
    val g2 = Gen.unit("B")
    val unionGen = Gen.union(g1, g2)

    var rng: RNG = SimpleRNG(42)
    val results = (1 to 1000).map { _ =>
      val (result, nextRng) = unionGen.sample(rng)
      rng = nextRng  
      result
    }

    val countA = results.count(_ == "A")
    val countB = results.count(_ == "B")

    assert(math.abs(countA - countB) < 100, s"Expected near equal distribution, but got countA = $countA and countB = $countB")
  }

    test("Gen.weighted should generate values from both generators over many samples") {
    val g1 = Gen.unit(1)
    val g2 = Gen.unit(2)
    val weightedGen = Gen.weighted((g1, 0.7), (g2, 0.3))

    var rng: RNG = SimpleRNG(42)
    val results = (1 to 100).map { _ =>
      val (result, nextRng) = weightedGen.sample(rng)
      rng = nextRng
      result
    }

    assert(results.contains(1) && results.contains(2), s"Expected results to contain both 1 and 2, but got $results")
  }

  test("Gen.weighted should produce approximately weighted distribution") {
    val g1 = Gen.unit("A")
    val g2 = Gen.unit("B")
    val weightedGen = Gen.weighted((g1, 0.7), (g2, 0.3))

    var rng: RNG = SimpleRNG(42)
    val results = (1 to 1000).map { _ =>
      val (result, nextRng) = weightedGen.sample(rng)
      rng = nextRng
      result
    }

    val countA = results.count(_ == "A")
    val countB = results.count(_ == "B")
    val ratio = countA.toDouble / (countA + countB)

    // 0.15 is kinda a magic number here, went off of vibes.
    assert(math.abs(ratio - 0.7) < 0.15, s"Expected ratio close to 0.7, but got $ratio")
  }

  import Prop._
  def passingProp: Prop = Prop((_, _) => Passed)
  def failingProp(message: String, successes: Int): Prop = Prop((_, _) => Falsified(message, successes))

  test("Prop.&& should pass if both properties pass") {
    val prop = passingProp && passingProp
    assertEquals(prop.run(10, SimpleRNG(1)), Passed)
  }

  test("Prop.&& should fail if the first property fails") {
    val prop = failingProp("First prop failed", 5) && passingProp
    assertEquals(prop.run(10, SimpleRNG(1)), Falsified("First prop failed", 5))
  }

  test("Prop.&& should fail if the second property fails") {
    val prop = passingProp && failingProp("Second prop failed", 8)
    assertEquals(prop.run(10, SimpleRNG(1)), Falsified("Second prop failed", 8))
  }

  test("Prop.|| should pass if the first property passes") {
    val prop = passingProp || failingProp("Should not be evaluated", 0)
    assertEquals(prop.run(10, SimpleRNG(1)), Passed)
  }

  test("Prop.|| should pass if the second property passes") {
    val prop = failingProp("First prop failed", 5) || passingProp
    assertEquals(prop.run(10, SimpleRNG(1)), Passed)
  }

  test("Prop.|| should fail if both properties fail") {
    val prop = failingProp("First prop failed", 5) || failingProp("Second prop failed", 8)
    assertEquals(prop.run(10, SimpleRNG(1)), Falsified("Second prop failed", 8))
  }

  test("unsized should produce the same result as the original Gen") {
    val gen = Gen.unit(42)
    val sgen = gen.unsized

    val rng = SimpleRNG(1)
    val (genResult, _) = gen.sample(rng)
    val (sgenResult, _) = sgen(10).sample(rng)  

    assertEquals(sgenResult, genResult, "SGen should produce the same result as Gen")
  }

  test("unsized SGen should ignore the size parameter") {
    val gen = Gen.unit(99)
    val sgen = gen.unsized

    val rng = SimpleRNG(1)
    val (result1, _) = sgen(1).sample(rng)  
    val (result2, _) = sgen(100).sample(rng) 

    assertEquals(result1, result2, "SGen should ignore size and produce the same result")
  }

  
  test("listOf should produce lists of the correct length") {
    val intGen = Gen.unit(42) 
    val listGen = Gen.listOf(Gen.unit(42))

    val rng = SimpleRNG(1)
    val listSize = 5
    val (result, _) = listGen(listSize).sample(rng) 

    assertEquals(result.length, listSize, s"Expected list of length $listSize, but got ${result.length}")
  }

  test("listOf should produce lists with elements generated by the original Gen") {
    val intGen = Gen.unit(7) 
    val listGen = Gen.listOf(Gen.unit(7))

    val rng = SimpleRNG(1)
    val (result, _) = listGen(3).sample(rng) 

    assert(result.forall(_ == 7), s"Expected all elements to be 7, but got $result")
  }

  test("listOf1 should produce lists with elements generated by the original Gen") {
    val intGen = Gen.unit(7) 
    val nonEmptyListGen = Gen.listOf(Gen.unit(7))

    val rng = SimpleRNG(1)
    val (result, _) = nonEmptyListGen(3).sample(rng) 

    assert(result.forall(_ == 7), s"Expected all elements to be 7, but got $result")
  }


  test("sorted list property") {
      val rng = SimpleRNG(1)
      val prop = Prop.forall(Gen.listOf(Gen.choose(-100, 100))(1000)) { list =>
      val sortedList = list.sorted
      val isOrdered = sortedList.zip(sortedList.tail).forall { case (a, b) => a <= b }
      val isPermutation = list.toSet == sortedList.toSet && list.size == sortedList.size
      isOrdered && isPermutation
    }
    
    println(prop.run(100, rng)) 
  }

}