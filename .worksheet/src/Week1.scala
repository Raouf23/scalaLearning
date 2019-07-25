object Week1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(58); 
  println("Welcome to the Scala worksheet");$skip(27); 
  val a = List(1, 2, 3, 4);System.out.println("""a  : List[Int] = """ + $show(a ));$skip(19); 
  val b = a.zip(a);System.out.println("""b  : List[(Int, Int)] = """ + $show(b ));$skip(72); 

  val rdd = for {
    x <- 1 to 3
    y <- 1 to 2
  } yield (x, None);System.out.println("""rdd  : scala.collection.immutable.IndexedSeq[(Int, None.type)] = """ + $show(rdd ));$skip(33); 
    
    
    val str  = "Hello";System.out.println("""str  : String = """ + $show(str ));$skip(30); 
    val disStr = str.distinct;System.out.println("""disStr  : String = """ + $show(disStr ));$skip(75); 
    
    val frequency =disStr.map(i => str.count(_ == i)).toList.distinct;System.out.println("""frequency  : List[Int] = """ + $show(frequency ));$skip(579); 
   // val minVal =frequency.min
   // frequency.map(x => x -minVal).sum
    def isValid(s: String): String = {
        val disStr = s.distinct
        val frequency =disStr.map(i => s.count(_ == i)).toList
        val minVal =frequency.min
        val sum =frequency.map(x => x -minVal).sum

        val disFreq = frequency.distinct
        val finalList = disFreq.map(x => frequency.count(_ == x)).toList

        if(finalList.distinct.size == 1) "YES"
        else if (finalList.distinct.size == 2 & ((finalList.head - finalList.tail.head) == 1))"YES"
        else "NO"

    };System.out.println("""isValid: (s: String)String""")}
    
}
