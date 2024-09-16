def g(l: Double, t: Double) : Double = l * ((2 * math.Pi ) / t) * ((2 * math.Pi ) / t)

def map2(l: Seq[Double], t: Seq[Double] )(f: (Double,Double) => Double): Seq[Double] = for (l1 <- l; t1 <- t) yield f(l1,t1)

val ls = Seq(0.5, 0.75, 1, 1.25, 1.5, 2, 5)
val ts = Seq(3.821, 4.800, 5.357, 6.222, 6.688, 7.644, 12.264)
val zs: Seq[(Seq[Double],Seq[Double])] = (ls zip ts) map (x => Seq(x._1) -> Seq(x._2))
val gs: Seq[Seq[Double]] = for (z <- zs) yield map2(z._1, z._2)(g)
val mean: Double = gs.foldLeft(0.0){ case (a,Seq(x)) => a + x} / gs.size

println( s"Mean: $mean" )