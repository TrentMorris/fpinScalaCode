trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG)
  def nextDouble(rng: RNG): (Double, RNG)
  def nextDouble2(rng: RNG): (Double, RNG)
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double, Double, Double), RNG)
  def map[A,B](s: Rand[A])(f: A =>B): Rand[B]
  def ints(count: Int)(rng: RNG):(List[Int], RNG)
}
type Rand[+A] = RNG => (A, RNG)
case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    val newN = scala.math.abs(n % Int.MaxValue)
    (newN, r)
  }
  def nextDouble(rng: RNG): (Double, RNG) = {
    val (n, r) = rng.nextInt
    (n.toDouble, r)
  }
  def nextDouble2(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r) = rng.nextInt
    ((n, n.toDouble), r)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (n, r) = rng.nextInt
    ((n.toDouble, n), r)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (n: Double, r: RNG) = rng.nextDouble(rng)
    ((n, n, n), r)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val x = for (i <- List.range(0, count)) yield { val (n, r) = rng.nextInt; n }
    val (s, y) = rng.nextInt
    (x, y)
  }
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)
}

