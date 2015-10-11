package anran.fuzzyDict

/**
 * Created by wanganran on 15/10/11.
 */
object Utils {
  def legal(s:String)= s.toLowerCase().filter {a=> (a>='a' && a<='z') || a==' ' || a=='-'}.map {_.toString}.foldLeft (""){_+_}
  def legalWildcard(s:String)=s.toLowerCase().replaceAll("_","?").filter {a=> (a>='a' && a<='z') || a==' ' || a=='-' || a=='*' || a=='?'}.map {_.toString}.foldLeft (""){_+_}
}
