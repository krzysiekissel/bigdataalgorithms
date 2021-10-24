import io.Source
import io.Codec
import java.nio.charset.CodingErrorAction


class WordCloudGenerator(var filename: String){
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val lines = Source.fromFile(filename)(decoder).getLines().toList        
    val stop_words = Source.fromFile("stop_words_english.txt")(decoder).getLines().toList 
    var words =Array[String]()
    for(line<-lines){
            val split = line.toLowerCase().replaceAll("\\p{Punct}", "").replaceAll("“","").split(' ')
            for (w<-split){
                if(!stop_words.contains(w.toLowerCase())){
                    if(w!=""){
                        words = words :+ w.toLowerCase()
                    }
                }
            }
    }
    private val word_count = collection.mutable.Map[String,Int]()
    for(word<-words){
        if(word_count.contains(word)){
                word_count(word)+=1
            }else{
            word_count+= (word->1)
        }
    }
    def print_word_cloud(number_of_words:Int):Unit={
        val sorted_seq = word_count.toSeq.sortWith(_._2>_._2)
        print(sorted_seq.take(number_of_words))
        print('\n')
    }
    def save_word_cloud(number_of_words:Int,title:String):Unit={
        val file_path =  title + ".csv"
        val pw = new java.io.PrintWriter(new java.io.File(file_path))
        val sorted_seq = word_count.toSeq.sortWith(_._2>_._2)
        for(s<-sorted_seq.take(number_of_words)){
            pw.write(s._2 + ", " + s._1 + '\n')
        }
        pw.close()
    }
}

object WordCloudGeneratorProgram{
    def main(args: Array[String]) = {
        
        println("Word Cloud Generator")
        var running = true
        while(running)
        {
            val menu_input = io.StdIn.readLine("SELECT OPTION (1 - load file    other key - exit):  ")
            if(menu_input == "1"){
                val file_input = io.StdIn.readLine("ENTER FILENAME:  ")
                val word_cloud_generator = new WordCloudGenerator(file_input)
                var generator_running = true
                while(generator_running){
                    val generator_menu_input = io.StdIn.readLine("SELECT OPTION (1 - print word cloud   2 - save word cloud     other key - exit):   ")
                    if(generator_menu_input == "1"){
                        val number_of_words_input = io.StdIn.readLine("ENTER NUMBER OF WORDS:   ")
                        word_cloud_generator.print_word_cloud(number_of_words_input.toInt)
                    }else if (generator_menu_input == "2"){
                        val number_of_words_input = io.StdIn.readLine("ENTER NUMBER OF WORDS:   ")
                        val output_filename_input = io.StdIn.readLine("ENTER OUTPUT FILENAME:   ")
                        word_cloud_generator.save_word_cloud(number_of_words_input.toInt,output_filename_input)
                    }else{
                        generator_running = false
                    }
                }
            }else{
                running = false
            }

        }
            

    }
}