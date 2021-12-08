import io.Source
import io.Codec
import java.nio.charset.CodingErrorAction
import scala.collection.mutable.Set
import scala.util.Random
import scala.util.control.Breaks._




object MinHashing{
    def main(args: Array[String]) = {
        val books = List("lotr1.txt","lotr2.txt","lotr3.txt","harry.txt","pap.txt","sherlock.txt")
        val booksAsString = books.map(readBook)
        val nHashes = Array(10,100,250,500)
        (4 to 13) foreach{k=>
            var shingles = booksAsString.map((x)=>kShingles(x,k)).toArray
            val unionedShingles = union_shingles(shingles)
            for(n<-nHashes){
                val sig = signatures(shingles,n)
                (0 to booksAsString.length-1) foreach{i=>
                    (0 to booksAsString.length-1) foreach{j=>
                        print(books(i))
                        print(" ")
                        print(books(j))
                        print(" ")
                        print(n)
                        print(" ")
                        print(k)
                        print(" ")
                        val sim = jaccardSignature(sig(i),sig(j))
                        print(sim)  
                        println()
                    }
                }
            }    
        }

    }
    def jaccardSignature(bookASignature:Array[Int],bookBSignature:Array[Int]):Float={
        return (0 to bookBSignature.length-1).map((i)=> if(bookBSignature(i)==bookASignature(i)) 1 else 0 ).sum.toFloat/bookBSignature.length.toFloat
    }
    def signatures(shingles:Array[Set[String]],nHashes:Int):Array[Array[Int]]={
         val rows = union_shingles(shingles)
         val columns = shingles.map((x)=>Array.fill(rows.length){0})
         (0 to rows.length-1) foreach {i=>
            (0 to columns.length-1) foreach {j=>
                val value = rows(i)
                if(shingles(j).contains(value)){
                    columns(j)(i) = 1
                }else{
                    columns(j)(i) = 0
                }
            }     
        }

        val signatures = (0 to nHashes-1).map((x)=>applyHash(columns))
        return (0 to columns.length-1).map((x)=>signatures.map((y)=>y(x)).toArray).toArray

    }

    def applyHash(matrix:Array[Array[Int]]):Array[Int]={
        val result = Array.fill(matrix.length){0}
        val perm = Random.shuffle(Array.range(0,matrix(0).length)).toArray
        (0 to matrix.length-1) foreach {i=>
            (0 to perm.length-1) foreach {j=>
                breakable { if(matrix(i)(perm(j))==1){
                    if(result(i)==0){
                        result(i)=j
                        break;
                    }
                    
                }}
            }    
        }
        return result
    }

    def union_shingles(shingles:Array[Set[String]]):Array[String]={
        var union_set = Set[String]()
        (0 to shingles.length-1) foreach {i=>
            union_set = union_set.union(shingles(i))
        }
        return union_set.toArray
    }
    def generateHashFunctions(n:Int,len:Int):Array[Array[Int]]={
        val range = Array.range(0,len)
        val functions = (0 to n).map((x)=> Random.shuffle(range).toArray).toArray
        return functions
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