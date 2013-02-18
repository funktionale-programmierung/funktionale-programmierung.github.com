package demo

object Mappers {
  type Mapper = String => List[String];
  
  def identityMapper(sourceFileName : String) =
    List(sourceFileName);

  def compositeMapper(mappers : List[Mapper]) =
    (sourceFileName : String) => {
      def f(m : Mapper) = m(sourceFileName)
      mappers.map(f).flatten
    }
  
  def chainedMapper(mappers : List[Mapper]) =
    (sourceFileName : String) => {
      def f(input : List[String], m : Mapper) =
        input.flatMap(m)
      mappers.foldLeft(List(sourceFileName))(f)
    }
   
  def globMapper(fromPat : String, toPat : String) = {
    val (fromPrefix, fromPostfix) = splitAtLastStar(fromPat)
    val (toPrefix, toPostfix) = splitAtLastStar(toPat)
    (sourceFileName : String) => {
      val (pre, mid, post) = splitPrePost(sourceFileName, fromPrefix.length(), fromPostfix.length())
      if ((pre == fromPrefix) && (post == fromPostfix))
        List(toPrefix + mid + toPostfix)
      else
        List()
    }
  }

  def splitAtLastStar(s : String) = {
    val p = s.lastIndexOf('*')
    if (p >= 0) {
      val pre = s.substring(0, p);
      val post = s.substring(p+1);
      (pre, post)
    }
    else (s, "")
  }

  def splitPrePost(s : String, preLen : Int, postLen : Int) = {
    val (pre, r) = s.splitAt(preLen);
    val (mid, post) = r.splitAt(r.length() - postLen);
    (pre, mid, post)
  }
}