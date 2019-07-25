
import scala.io.Source
import java.io._
import scala.io.BufferedSource

object FileGenerator {

  def main(args: Array[String]): Unit = {
 val startTime = System.nanoTime
    val input = Source.fromFile("C:\\Users\\raokhan\\workspace\\ScalaLearning\\input\\mappingInput.txt")
    val file = new File("C:\\Users\\raokhan\\workspace\\ScalaLearning\\output\\mappingOutput.txt")
    val pw = new BufferedWriter(new FileWriter(file, true))

    val addString = """'additional_col' : [col("post_event_list"), col("post_evar26"),col('post_prop28'),col('post_prop1'),col('post_prop62')],
			'events'	: [F.when(array_contains(split(col('post_event_list'),','),'255'), 'TDS').otherwise(0),
                           F.when(array_contains(split(col('post_event_list'),','),'259'), 'TDC').otherwise(0),
                           F.when(array_contains(split(col('post_event_list'),','),'216'), 'CCS').otherwise(0),
                           F.when(array_contains(split(col('post_event_list'),','),'223'), 'CCE').otherwise(0),
                           F.when(array_contains(split(col('post_event_list'),','),'256'), 'TDSM').otherwise(0),
						   lit(0), #CBS
						   lit(0), #RQS"""

      pw.write(addString +"\n")

    for (line <- input.getLines()) {
            if (line.contains("post_event_list")) {
        val eventValue = eventNumGenerator(line.split("\t")(7).substring(5).toInt)
        val eventName = line.split("\t")(1)
        pw.write("F.when(array_contains(split(col('post_event_list'),','),'" + eventValue + "'), '" + eventName + "').otherwise(0)," + "\n")
      }

 

      if (line.contains("post_eVar26")) {
        val eventName = line.split("\t")(1)
        val eventCondition = line.split("\t")(7)
        if (eventCondition.contains(" or ")) {
          val conditions = eventCondition.split("or")
          pw.write("F.when(")
          conditions.map(x => pw.write("(col('post_evar26').contains('"+x.trim()+"')) | "))
          pw.write(",'" + eventName + "').otherwise(0)," + "\n")
        } 
        else if (eventCondition.contains('&')) {
          val conditions = eventCondition.split("&")
          pw.write("F.when(")
          conditions.map(x => pw.write("(col('post_evar26').contains('"+x.trim()+"')) & "))
          pw.write(",'" + eventName + "').otherwise(0)," + "\n")
        } 
        else {
          pw.write("F.when((col('post_evar26') == '" + eventCondition + "'),'" + eventName + "').otherwise(0)," + "\n")
          }
      }
         if (line.contains("post_prop1")) {
        val eventName = line.split("\t")(1)
        val eventCondition = line.split("\t")(7)
        if (eventCondition.contains("contains")) {
          val newCondition = eventCondition.substring(9)
          pw.write("F.when((col('post_prop1').contains('" + newCondition + "'),'" + eventName + "').otherwise(0)," + "\n")
        } else
          pw.write("F.when((col('post_prop1') == '" + eventCondition + "'),'" + eventName + "').otherwise(0)," + "\n")
      }

         if (line.contains("post_prop28")) {
        val eventName = line.split("\t")(1)
        val eventCondition = line.split("\t")(7)
        if (eventCondition.contains("contains")) {
          val newCondition = eventCondition.substring(9)
          pw.write(("F.when((col('post_prop28').contains('" + newCondition + "'),'" + eventName + "').otherwise(0),").replaceAllLiterally("''", "'") + "\n")
        } else
          pw.write("F.when((col('post_prop28') == '" + eventCondition + "'),'" + eventName + "').otherwise(0)," + "\n")
      }

    }

    pw.close()
    
     val input1 = Source.fromFile("C:\\Users\\raokhan\\workspace\\ScalaLearning\\input\\mappingInput.txt")
    val file1 = new File("C:\\Users\\raokhan\\workspace\\ScalaLearning\\output\\mappingOutput.txt")
    val bw = new BufferedWriter(new FileWriter(file1, true))
    
    
    val input2 = Source.fromFile("C:\\Users\\raokhan\\workspace\\ScalaLearning\\input\\mappingInput.txt")
    val file2 = new File("C:\\Users\\raokhan\\workspace\\ScalaLearning\\output\\mappingOutput.txt")
    val bw2 = new BufferedWriter(new FileWriter(file2, true))
    
    bw2.write("       ]+ generic_events," + "\n")
    bw2.write("'lead_col' : F.when((array_contains(split(col(\"CONVERSION_EVENTS\"),\",\"),\"TDC\") |")
    addLeadFlag(input2, file2, bw2)
    bw2.write(") , 1).otherwise(0),"+"\n")
    bw2.close()
    bw.write("'engaged_col' : F.when((generic_engaged_criteria | array_contains(split(col(\"CONVERSION_EVENTS\"),\",\"),'TDS')  | array_contains(split(col(\"CONVERSION_EVENTS\"),\",\"),'TDSM') | array_contains(split(col(\"CONVERSION_EVENTS\"),\",\"),'SOCL') |")
    addEngageFlag(input1, file1, bw)
    bw.write("),1).otherwise(0)")
    bw.close
    println("File generated")
    val stopTime = System.nanoTime
    val delta = stopTime - startTime
    println("Time taken -:" + delta / 1000000d)
  }

  def eventNumGenerator(eventNum: Int): String = {
    if (eventNum < 100)
      ("2" + (eventNum - 1).toString)
    else
      ("20" + (eventNum - 1).toString)
  }

  def addEngageFlag(input: BufferedSource, file: File, pw: BufferedWriter) = {
    for (line <- input.getLines()) {
      if (line.split("\t")(4) == "1") {
        val eventName = line.split("\t")(1)
        pw.write("array_contains(split(col(\"CONVERSION_EVENTS\"),\",\"),'" + eventName + "') |" + "\n")
      }
    }
  }

  
   def addLeadFlag(input: BufferedSource, file: File, pw: BufferedWriter) = {
    for (line <- input.getLines()) {
      if (line.split("\t")(3) == "1") {
        val eventName = line.split("\t")(1)
        pw.write("(array_contains(split(col(\"CONVERSION_EVENTS\"),\",\"),'" + eventName + "') |" + "\n")
      }
    }
  }
  def timer[A](blockOfCode: => A) = {
    val startTime = System.nanoTime
    val result = blockOfCode // the "block of code" you pass in is run here
    val stopTime = System.nanoTime
    val delta = stopTime - startTime
    (result, delta / 1000000d) // return the result and time as a Tuple
  }
}



