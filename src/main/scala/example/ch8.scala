package ch8 
import example.Par.Par
import example.Par
import java.util.concurrent._

// Ch6 stuff
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s1) = run(s)
    (f(a), s1)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }
}

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  // Define a type alias for State transformations on RNG
  type Rand[+A] = State[RNG, A]

  def int: Rand[Int] = State(_.nextInt)

  def nonNegativeInt: Rand[Int] = int.map(n => if (n < 0) -(n + 1) else n)

  def double: Rand[Double] = nonNegativeInt.map(n => n / (Int.MaxValue.toDouble + 1))

  def intDouble: Rand[(Int, Double)] = int.flatMap(i => double.map(d => (i, d)))

  def doubleInt: Rand[(Double, Int)] = double.flatMap(d => int.map(i => (d, i)))
}

/* 
  8.1 
  - sum(a) + sum(b) should equal sum(a ++ b)
  - sum(List()) == 0
  - sum(c * X) == c * sum(X)
    (In scala terms, sum(X.map(val => val * c)) == c * sum(X))
  - sum(a :: x) == sum(a) + x
  Plenty of others, I could do this all day
*/

/* 
  8.2
  - max(A :: B) == max(max(a), max(b))
  - max(List(a)) == a
  - max(A) == max(A.reverse) (order independence)
  etc etc etc
*/


case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {
  // To avoid needing to prefix this all the time 
  import Prop._

  def &&(p: Prop): Prop = Prop { (testCases, rng) =>
    this.run(testCases, rng) match {
      case Passed => p.run(testCases, rng) // If this property passes, evaluate the second property
      case falsified: Falsified => falsified // If this property fails, short-circuit and return failure
    }
  }

  def ||(p: Prop): Prop = Prop { (testCases, rng) =>
    this.run(testCases, rng) match {
      case Passed => Passed // If this property passes, return success immediately
      case Falsified(_, _) => p.run(testCases, rng) // If this property fails, evaluate the second property
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forall[A](gen: Gen[A])(predicate: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) =>
        try {
          if (predicate(a)) Passed else Falsified(a.toString(), i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}


case class Gen[+A](sample: RNG => (A, RNG)) {
  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen { rng =>
      val (a, nextRng) = sample(rng)
      f(a).sample(nextRng)
    }
  }

  // pretty sweet.
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  // just return self ignoring size
  // 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

// 8.11
case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { f(_).g(n) }
    }
    SGen(g2)
  }
}

object Gen {
  //8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    require(start < stopExclusive, "start must be less than stopExclusive")
    Gen { rng =>
      val (n, nextRng) = rng.nextInt
      val scaled = start + Math.abs(n % (stopExclusive - start))
      (scaled, nextRng)
    }
  }

  def unit[A](a: => A): Gen[A] = {
    // doesn't generate a new rng. Probably makes the most sense to do that, right?
    Gen { rng => 
      (a, rng)
    }
  }

  def boolean: Gen[Boolean] = {
    Gen { rng =>
      val (n, nextRng) = rng.nextInt
      (n % 2 == 0, nextRng) 
    }
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen { rng =>
      def loop(count: Int, rng: RNG, acc: List[A]): (List[A], RNG) = {
        if (count <= 0) (acc, rng)
        else {
          val (a, nextRng) = g.sample(rng)
          loop(count - 1, nextRng, a :: acc)
        }
      }

      loop(n, rng, List.empty)
    }
  }

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen { rng =>
      val (choice, nextRng) = rng.nextInt
      if (choice % 2 == 0) g1.sample(nextRng) else g2.sample(nextRng)
    }
  }

  //8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2
    val totalWeight = weight1 + weight2

    Gen { rng =>
      val (choice, nextRng) = rng.nextInt
      val threshold = (weight1 / totalWeight) * Int.MaxValue
      
      if (choice.toDouble < threshold) gen1.sample(nextRng)
      else gen2.sample(nextRng)
    }
  }

  // 8.12: listOf method that returns an SGen
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => Gen.listOfN(size, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(size => Gen.listOfN(size max 1, g))

  //8.17
  val forkProp: Prop = Prop.forall(Gen.choose(-100, 100)){ (x: Int) =>
    val es = Executors.newFixedThreadPool(2)
    
    val parX: Par[Int] = Par.unit(x)
    
    Par.equal(es)(parX, Par.fork(parX))
  }
}

/*
  8.15
  - For a finite domain, you'd first need to figure out all values within that domain. 
    - Not sure if there's a good a good way to do this for an arbitrary type. You could hardcode 
      clearly known ones, but I don't think there's a good way to do do this automatically.
    - After you have all values in the domain, simply try them all. For multi-param functions where 
      both are of finite domain, generate all permutations to exhaustively check the property.
    - sized generator follows same kind of principle. For size n, You can exhaustively generate 
      all n cases (so for positive integers as the type, 0 -> n). Then you run your your stuff 
      on each one. Again, the difficulty here is type-independent generation. If you just hardcode 
      the type value generators (so when you want positive ints, we start with 0, 1, 2, etc), this 
      wouldn't be too bad.
*/

/*
  8.18
  - Properties
    - The result of takeWhile should always be prefix of the original list
    - If all elements satisfy the input function, the result should be the entire list 
    - If takeWhile stops at x, all subsequent elements after x shouldn't be in the result 
      - dropWhile is the opposite; if it stops at x, all subsequent elements should be in the result
*/

/*
  8.19
  - We want to generate a functions that vary based on inputs. So you provide an int generator,
    and it'll give you a generator capable of generating functions that go str => int.
    - The most simple option for gen[Int]  -> Gen[String => Int] would be 
      to hash the string and use that to return an int. So like 
      g.map(i => (s.hashCode + i)) or something like that  
    - This kills all variation and "meaning" though. So if you wanted longer 
      strings to correspond to larger ints or something, that wouldn't work.
    - To continue this point, there's just not really a relationship between the 
      input and output.


  - Another option is to do something rule based. Ex:
    def genStringIntFn(g: Gen[Int]): Gen[String => Int] = {
      g.flatMap { i =>
        Gen.oneOf(
          (s: String) => s.length + i,
          (s: String) => s.map(_.toInt).sum + i,
          whatever other random thing
        )
      }
    }
      This is a very inflexible approach, but it keeps meaning. 
*/



object Main {
  def main(args: Array[String]): Unit = {
    println("testing")
  }
}