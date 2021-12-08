import io.Source
import io.Codec
import java.nio.charset.CodingErrorAction
import scala.collection.mutable.Set




object DocumentsSimilarity{
    def main(args: Array[String]) = {
        val books = List("lotr1.txt","lotr2.txt","lotr3.txt","harry.txt","pap.txt","sherlock.txt")
        val booksAsString = books.map(readBook)
        (0 to booksAsString.length-1) foreach{i=>
            (0 to booksAsString.length-1) foreach{j=>
                (4 to 13) foreach{k=>
                   print(books(i))
                   print(" ")
                   print(books(j))
                   print(" ")
                   print(k)
                   print(" ")
                   print(jaccardSimilarity(booksAsString(i),booksAsString(j),k))  
                   println()   
                }
            }
        }
    }
    def readBook(filename:String):String={
        val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
        return Source.fromFile(filename)(decoder).getLines.mkString.replace(" ","")  
    }

    def jaccardSimilarity(bookA:String, bookB:String,k:Int):Float={
        val bookAShingles = kShingles(bookA,k)
        val bookBShingles = kShingles(bookB,k)
        return jaccardFull(bookAShingles,bookBShingles)
    }

    def kShingles(book:String,k:Int):Set[String]={
        var shingles = Set[String]()
        (0 to (book.length-k)) foreach {i=>
            val shing = book.substring(i,i+k)
            shingles+=shing
        }
        return shingles
    }

    def jaccardFull(bookAShingles:Set[String],bookBShingles:Set[String]):Float={
        val intersection = bookAShingles.intersect(bookBShingles)
        val union = bookAShingles.union(bookBShingles)
        return intersection.size.toFloat/union.size.toFloat
    }
}