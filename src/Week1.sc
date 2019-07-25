object Week1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val a = List(1, 2, 3, 4)                        //> a  : List[Int] = List(1, 2, 3, 4)
  val b = a.zip(a)                                //> b  : List[(Int, Int)] = List((1,1), (2,2), (3,3), (4,4))

  val rdd = for {
    x <- 1 to 3
    y <- 1 to 2
  } yield (x, None)                               //> rdd  : scala.collection.immutable.IndexedSeq[(Int, None.type)] = Vector((1,N
                                                  //| one), (1,None), (2,None), (2,None), (3,None), (3,None))
    
    
    val str  = "Hello"                            //> str  : String = Hello
    val disStr = str.distinct                     //> disStr  : String = Helo
    
    val frequency =disStr.map(i => str.count(_ == i)).toList.distinct
                                                  //> frequency  : List[Int] = List(1, 2)
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

    }                                             //> isValid: (s: String)String
    
}