package anran.fuzzyDict

import anran.fuzzyDict.DictIndex

import scala.annotation.tailrec
import scala.concurrent.Promise

/**
 * Created by wanganran on 15/10/11.
 */
object Shared{
  val wordPath="/Users/wanganran/voc2.txt"

  var count=0
  var fut=Promise[Int]

  def using[T,R](x:T)(y:T=>R)=y(x)
  def loadDict()= {
    val file = scala.io.Source.fromFile(wordPath).getLines()
    @tailrec
    def create(it: Iterator[String], last: List[(String,String)]): List[(String, String)] =
      (if (it.hasNext)
        using(it.next()) { s =>
          val ss=Utils.legal(s)
          if (it.hasNext)
            Some(ss, "<p><b>"+s+"</b></p>"+it.next())
          else
            Some(ss, "")
        }
      else None)
      match{
        case Some(t) =>
          create(it, last.::(t))
        case None => last
      }

    create(file, List[(String,String)]()).toArray

  }

  lazy val index=new DictIndex(loadDict())
}
