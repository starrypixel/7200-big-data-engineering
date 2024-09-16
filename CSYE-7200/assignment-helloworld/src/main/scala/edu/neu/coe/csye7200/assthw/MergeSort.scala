package edu.neu.coe.csye7200.assthw

import scala.annotation.tailrec

/**
 * This is the lazy version of MergeSort.
 * The difference is in the treatment of result in the inner method "merge."
 *
 * @tparam X underlying type that must support Ordering via an implicit value.
 */
class MergeSort[X: Ordering] {

    def sort(xs: List[X]): List[X] = xs match {
        case Nil => xs
        case _ :: Nil => xs
        case _ =>
            @tailrec
            def merge(result: List[X], l: List[X], r: List[X]): List[X] =
                (l, r) match {
                    case (Nil, _) => result.reverse ++ r // NOTE: necessary to reverse result.
                    case (_, Nil) => result.reverse ++ l // NOTE: necessary to reverse result.
                    case (h1 :: t1, h2 :: t2) =>
                        if (implicitly[Ordering[X]].compare(h1, h2) <= 0)
                            merge(h1 :: result, t1, r) // NOTE: fast but result will be backwards
                        else
                            merge(h2 :: result, l, t2) // NOTE: fast but result will be backwards
                }

            val (l, r) = xs.splitAt(xs.length / 2)
            val (ls, rs) = (sort(l), sort(r))
            merge(Nil, ls, rs)
    }
}

object MergeSort extends App {

    def doMain(n: Int): Seq[Int] = {
        val sorter = new MergeSort[Int]
        val list = (1 to n).toList.reverse
        sorter.sort(list)
    }

    println(doMain(100000))
}