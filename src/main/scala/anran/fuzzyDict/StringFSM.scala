package anran.fuzzyDict

import scala.util.matching.Regex

/**
 * Created by wanganran on 15/10/6.
 */
abstract class StringFSM[S] {
  def input(str:String):S
  def reset()
}

class WildcardState
case object Accept extends WildcardState
case object Denied extends WildcardState
object StringFSM {
  def FSMFromWildcard(wildcard: String) = {
    /*
    class State
    case class InterState(var permit: PermitLetter, var prevState:State, var nextState:State, var failOverState:State) extends State
    case class FirstState(var permit: PermitLetter, var nextState:State) extends State
    case object TerminateState extends State


    class PermitLetter
    case object AnyLetter extends PermitLetter
    case class OneLetter(ch:Char) extends PermitLetter
    case class OneLetterSpin(ch:Char) extends PermitLetter
    case object AnyLetterSpin extends PermitLetter

    def using[T, R](t: T)(x: T => R) = x(t)

    def makeFSM=using(wildcard.zipWithIndex.map {
      case ('?', _) => AnyLetter
      case ('*', i) =>
        if (i == wildcard.length - 1) AnyLetterSpin
        else OneLetterSpin(wildcard(i + 1))
      case (ch, i) => if (i != 0 && wildcard(i - 1) != '*') OneLetter(ch) else null
    }.filter(_!=null)){letters =>
      val escape=Array.fill(letters.length){0}
      for(idx<-0 until letters.length) escape(idx)=(letters(idx), idx) match {
        case (AnyLetter, idx) => idx
        case (OneLetterSpin(ch), idx) => idx
        case (AnyLetterSpin, idx) => idx
        case (OneLetter(ch), idx) =>
          val t=letters(idx-1)
          def eq(a:PermitLetter,b:PermitLetter)=(a,b) match{
            case (AnyLetter,_)=>true
            case (_,AnyLetter)=>true
            case (OneLetter(ch1), OneLetter(ch2))=>ch1==ch2
            case (OneLetter(ch1), OneLetterSpin(ch2))=>ch1==ch2
            case (OneLetterSpin(ch1), OneLetter(ch2))=>ch1==ch2
            case (OneLetterSpin(ch1), OneLetterSpin(ch2))=>ch1==ch2
            case _=>false
          }
          while(!eq(t, escape()))
      }

    }}}

*/
    val regexExp="^" + wildcard.replaceAll("\\*", ".*").replaceAll("\\?", ".").replaceAll("_",".") + "$"
    println(regexExp)
    val regex = new Regex(regexExp)

    new StringFSM[WildcardState] {

      def input(str: String) = if (regex.findAllIn(str).nonEmpty) Accept else Denied

      def reset() {}
    }
  }
}
