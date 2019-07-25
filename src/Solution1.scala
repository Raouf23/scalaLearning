import scala.collection.mutable

object Solution1 {
  
  def main(args: Array[String]): Unit = {
    
   println(countChar("Hello"))
    
    def countChar(s:String) = {
       val charCount = mutable.Map[Char,Int]()
      for(c  <- s) {
        val character = c.toLower
       val oldCount =  if( charCount.contains(character)) charCount(character) else 0
       charCount += (character -> (oldCount+ 1))
      } 
       charCount
    }
   
   
   
   
   def matchedChar(str1 :String, str2:String) = {
     
     val map1 = countChar(str1)
     val map2 = countChar(str2)
     var deletion = 0
     map1.map(f => if(map2.contains(f._1)) deletion = deletion +Math.abs(map2(f._1)-f._2) ) 
     val map1Diff = map1.filter(f => !map2.contains(f._1)) 
     val map2diff = map2.filter(f => !map1.contains(f._1))
     
     deletion + map1Diff.values.sum + map2diff.values.sum
   }
   
   
  println( matchedChar("jxwtrhvujlmrpdoqbisbwhmgpmeoke", "fcrxzwscanmligyxyvym"))
  }
}