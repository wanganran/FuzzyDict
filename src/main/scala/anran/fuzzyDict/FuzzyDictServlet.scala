package anran.fuzzyDict

import org.scalatra._
import scalate.ScalateSupport

import scala.xml.NodeSeq

class FuzzyDictServlet extends FuzzyDictStack {

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.
      </body>
    </html>
  }
  get("/dict/exact/:content"){
    Shared.index.exactIndex(Utils.legal(params("content"))).take(10).map{x => <p>{x}</p>}.foldLeft (NodeSeq.Empty){_++_}
  }
  get("/dict/startwith/:content"){
    Shared.index.prefixIndex(Utils.legal(params("content"))).take(10).map{x => <p>{x}</p>}.foldLeft (NodeSeq.Empty){_++_}
  }
  get("/dict/wildcard/:content"){
    println(params("content"))
    Shared.index.wildcardIndex(Utils.legalWildcard(params("content"))).take(10).map{x => <p>{x}</p>}.foldLeft (NodeSeq.Empty){_++_}
  }
  get("/dict/endwith/:content"){
    Shared.index.suffixIndex(Utils.legal(params("content"))).take(10).map { x=> <p>{x}</p> }.foldLeft (NodeSeq.Empty){_++_}
  }
  get("/dict/dist/:len/:content"){
    val len=try {
      params("len").toInt
    }
    catch {
      case _:Throwable => -1
    }
    if(len<0 || len>Shared.index.maxEditDist)NotFound("Length not legal.")
    else
      Shared.index.editDistIndex(len)(Utils.legal(params("content"))).take(10).map {case (x,_)=> <p>{x}</p> }.foldLeft (NodeSeq.Empty) {_++_}
  }

}
