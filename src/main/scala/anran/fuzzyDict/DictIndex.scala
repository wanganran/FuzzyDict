package anran.fuzzyDict

/**
 * FuzzyDict index algorithm
 * Created by wanganran on 15/10/5.
 */

import Ordering.Implicits._

object MainObject{
  def main (args: Array[String]): Unit = {
    //Shared.index.lengthAndOneCharIndex(5)(3,'a').foreach(println(_))
    print(Shared.index.wildcardIndex("abs*").foreach(println))
    //println(Shared.index.editDist("wit","lis"))
  }
}

class DictIndex (dict: Array[(String, String)]) {

  private val LIMIT = 3
  private val words = dict.map { case (word, translate) => word }

  private def firstInSorted(s: String, arr: Array[String], start: Int = 0, _end: Int = -1): Option[Int] = {
    val end = (arr.length + _end) % arr.length

    if (start > end) None
    else if (start == end)
      if (arr(start).startsWith(s)) Some(start) else None
    else {
      val mid = (start + end) / 2
      if (arr(mid).startsWith(s)) firstInSorted(s, arr, start, mid)
      else if (arr(mid) > s) firstInSorted(s, arr, start, mid)
      else firstInSorted(s, arr, mid + 1, end)
    }
  }

  private def using[T, R](t: T)(x: T => R) = x(t)

  private def prefixOrder(s: String) = s

  private def suffixOrder(s: String) = s.reverse

  val translateIndex = using(dict.foldLeft(Map.empty[String, String]) { (map, tuple) => map + tuple }) { map =>
    map(_)
  }

  private def find(index: Array[String]) = {
    word: String => firstInSorted(word, index) map {
      index.iterator.drop(_).takeWhile(_.startsWith(word))
    } getOrElse Iterator[String]()
  }


  //String=>Iterator[String]
  val prefixIndex = using(words.sortBy(
    prefixOrder
  ))(
      find
    )

  //String=>Iterator[String]
  val exactIndex = {
    s: String => using(prefixIndex(s)) { it => if (it.isEmpty) it else using(it.next()) { ss => if (ss != s) Iterator[String]() else Iterator(ss) }
    }
  }

  //String=>Iterator[String]
  def suffixIndex = using(words.map {_.reverse}.sortBy(
    prefixOrder
  )) { arr =>
    s: String => find(arr)(s).map {
      _.reverse
    }
  }

  //Int=>Iterator[String]
  val lengthIndex = using(words.groupBy(_.length)) { groups => len: Int => groups.get(len).map(_.iterator).getOrElse(Iterator[String]()) }

  //Int,Char=>Iterator[String]
  val oneCharIndex = using((0 to LIMIT).map { pos =>
    ('a' to 'z').map { ch =>
      words.filter(s => using(Math.min(s.length - 1, pos)) { l => (0 to l).map(s.charAt(_) == ch).foldLeft(false)(_ & _) })
    }.toArray
  }.toArray) { idx => (pos: Int, ch: Char) =>
    if (pos < idx.length)
      using(idx(pos)(ch - 'a')) { _.iterator }
    else Iterator[String]()
  }

  //Map[Int, (Int,Char)=>Iterator[String]]
  val lengthAndOneCharIndex = using(words.groupBy(_.length)) {
    _.map { case (length, arr) =>
      val limit = Math.min(LIMIT, length - 1)
      using((0 to limit).map { pos =>
        Map((('a' to 'z')++ Array('-',' ')).map { ch => ch ->
          arr.filter(s => s.length >= pos && (0 to pos).map(s.charAt(_) == ch).foldLeft(false)(_ | _)).sortBy(prefixOrder)
        } :_*)
      }.toArray) { idx =>
        (length, (pos: Int, ch: Char) => if (pos < idx.length)
          using(idx(pos)(ch)) { _.iterator }
        else Iterator[String]())
      }
    }
  }


  private case class LazyMat[T](w: Int, h: Int, func: (LazyMat[T], Int, Int) => T) extends ((Int, Int) => T) {
    var buffer = Array.fill[Option[T]](w, h) {
      None
    }

    override def apply(x: Int, y: Int): T = {
      buffer(x)(y) match {
        case None => using(func(this, x, y)) { r => buffer(x)(y) = Some(r); r }
        case Some(r) => r
      }
    }
  }

  def editDist(str1: String, str2: String): Int = {
    val dp = LazyMat[Int](str1.length+1, str2.length+1, (self, x, y) =>
      if (Math.abs(x - y) > LIMIT) LIMIT + 1
      else if (x == 0) y
      else if (y == 0) x
      else
        using(Math.min(self(x - 1, y), self(x, y - 1)) + 1) { m =>
          if (str1(x-1) == str2(y-1))
            Math.min(m, self(x - 1, y - 1))
          else
            Math.min(m, self(x - 1, y - 1) + 1)
        })
    val res=dp(str1.length, str2.length)
    res
  }

  private def merge[A: Ordering](a: Iterator[A], b: Iterator[A]): Stream[A] = (a.hasNext, b.hasNext) match {
    case (true, true) => using(a.next()) { ai =>
      using(b.next()) { bi =>
        if (ai < bi) ai #:: merge(a, Iterator(bi) ++ b)
        else if (ai == bi) ai #:: merge(a, b)
        else bi #::merge(Iterator(ai) ++ a, b)
      }
    }
    case (true, false) => using(a.next()) { ai => ai #:: merge(a, b) }
    case (false, true) => using(b.next()) { bi => bi #:: merge(a, b) }
    case (false, false) => Stream.empty[A]
  }

  private def zigzag(len: Int, min: Int, max: Int): Iterator[Int] = {
    Iterator(len) ++
      using(Math.min(len - min, max - len)) { d =>
        (1 to d).flatMap { d => Iterator(len - d, len + d) } ++
          (min until len - d) ++
          (len + d + 1 to max)
      }
  }

  def maxEditDist=LIMIT
  //String=>Iterator[(String,Int)]
  def editDistIndex(maxDist: Int) = {
    (str: String) =>
      (for (i <- zigzag(str.length, Math.max(str.length - maxDist, 1), str.length + maxDist)) yield {
        (lengthAndOneCharIndex.get(i) match {
          case Some(idx) =>
            val diff = str.length - i
            if (diff >= 0)
              for (j <- 0 until Math.min(str.length, maxDist + 1)) yield idx(Math.min(maxDist - diff, i - 1), str.charAt(j))
            else
              for (j <- 0 until Math.min(str.length, maxDist + diff + 1)) yield idx(Math.min(maxDist - 1, i - 1), str.charAt(j))

          case None => Nil

        }).foldLeft(Iterator[String]()){(t, arr) =>/* arr.foreach(x=>print(x+" ")); println;*/ merge(t, arr).iterator}
          .map{candidate => (candidate, editDist(str, candidate))}
          .filter { case (_, dist) => dist <= maxDist }

      }).foldLeft(Iterator[(String, Int)]()) {
        _ ++ _
      }

  }

  //Map[String, Array[String]]
  val twoLetterIndex = Map((for (i <- 'a' to 'z'; j <- 'a' to 'z') yield i.toString + j.toString).map(pat => (
    pat, words.filter(_.contains(pat)))
  ): _*)

  //String=>Iterator[String]
  val wildcardIndex = (wildcard: String) => {
    def removeMinus(x: Int) = if (x < 0) Int.MaxValue else x
    val pos = Math.min(removeMinus(wildcard.indexOf('*')), removeMinus(wildcard.indexOf('?')))
    if (pos == Int.MaxValue) exactIndex(wildcard)
    else {
      var regular = wildcard
      var old: String = null
      while (old != regular) {
        old = regular
        regular = regular.replace("**", "*")
      }
      old = null
      while (old != regular) {
        old = regular
        regular = regular.replace("*?", "?*")
      }

      val FSM = StringFSM.FSMFromWildcard(regular)
      val arr = (Array(
        if (!(regular.startsWith("*") || regular.startsWith("?"))) {
          val pos = Math.min(removeMinus(regular.indexOf('*')), removeMinus(regular.indexOf('?')))
          Some(prefixIndex(regular.substring(0, pos)))
        }
        else None,
        if (!(regular.endsWith("*") || regular.endsWith("?"))) {
          val pos = Math.max(regular.lastIndexOf('*'), regular.lastIndexOf('?'))
          Some(suffixIndex(regular.substring(pos + 1)))
        }
        else None) ++
        regular.zipWithIndex.map { case (ch, idx) =>
          if (idx == regular.length - 1) null else ch.toString + regular.charAt(idx + 1).toString }
          .filter(s => s != null && s(0) != '*' && s(1) != '*' && s(0) != '?' && s(1) != '?')
          .map(twoLetterIndex)
          .map(a => Some(a.iterator))
        ).collect { case Some(it) => it }
      if (arr.isEmpty) {
        val minLength = wildcard.count(_ != '*')
        words.sortBy(prefixOrder).iterator.filter(_.length >= minLength).map(s => (s, FSM.input(s))).collect { case (s, Accept) => s }
      }
      else {
        arr(0).map(s => (s, FSM.input(s))).collect { case (s, Accept) => s }
      }


    }
  }

}
