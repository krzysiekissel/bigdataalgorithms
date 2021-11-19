import io.Source
import io.Codec
import java.nio.charset.CodingErrorAction
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map



object WordCloudGeneratorProgram{
    def main(args: Array[String]) = {
        val files = List[String]("book.txt","book2.txt")
        var word_counts = generate_word_counts(files)
        word_counts.foreach(s=>print_word_cloud(s._2,10))
        var tfidfs = generate_tfidfs(word_counts)
        tfidfs.foreach(s=>print_tfidfs(s._2,10))
            

    }
    def print_word_cloud(word_count:Map[String,Int],number_of_words:Int):Unit={
        val sorted_seq = word_count.toSeq.sortWith(_._2>_._2)
        print(sorted_seq.take(number_of_words).map(x => (x._2,x._1)))
        print('\n')
    }
    def print_tfidfs(tfidfs:Map[String,Double],number_of_words:Int):Unit={
        val sorted_seq = tfidfs.toSeq.sortWith(_._2>_._2)
        print(sorted_seq.take(number_of_words).map(x => (x._2,x._1)))
        print('\n')
    }
    def generate_tfidfs(word_counts: Map[String,Map[String,Int]]): Map[String,Map[String,Double]]={
        return word_counts.map(x=>(x._1,tfidf(x._2,word_counts)))
    }
    def tfidf(word_count:Map[String,Int],word_counts: Map[String,Map[String,Int]]): Map[String,Double]={
        val tf_map = tf(word_count)
        val idf_map = idf(word_count,word_counts)
        return tf_map.map(x=>(x._1,x._2*idf_map(x._1)))
    }
    def idf(word_count:Map[String,Int],word_counts:Map[String,Map[String,Int]]):Map[String,Double]={
        val number_of_documents = word_counts.size
        return word_count.map(x=>(x._1,math.log(number_of_documents/number_of_documents_containing_term(x._1,word_counts))))
    }
    def number_of_documents_containing_term(term:String,word_counts:Map[String,Map[String,Int]]):Int={
        var counter = 0
        for(word_count<-word_counts){
            if(word_count._2.contains(term)){
                counter+=1
            }
        }
        return counter
    }
    def tf(word_count:Map[String,Int]):Map[String,Double]={
        val document_length  = word_count.foldLeft(0)(_+_._2).toDouble
        return word_count.map(x=>(x._1,x._2/document_length))
    }

    def generate_word_counts(filenames: List[String]) : Map[String,Map[String,Int]]={
        val word_counts = Map[String,Map[String,Int]]()
        for(filename<-filenames){
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
            val word_count = Map[String,Int]()
            for(word<-words){
                if(word_count.contains(word)){
                        word_count(word)+=1
                    }else{
                    word_count+= (word->1)
                }
            }
            word_counts+=(filename->word_count)
        }
        
        return word_counts
    }
}



