import scala.collection.mutable.Map
import io.Source

object Graph{
    def main(args: Array[String]) = {
        val result = Source.fromFile("web-Stanford.txt").getLines.map(mapper).reduce(reducer)
        println()
        for(v<-result){
             print(v._1)
             print(" ")
             print(v._2)
             print(" ")
             print(v._3)
             print(" ")
             println()
        }
        
    }

    def mapper(arg:String):Array[(Int,Int,Int)]={
        val nodes = arg.split("\t").map((x)=>x.toInt).toArray
        return Array((nodes(0),0,1))++Array((nodes(1),1,0))
    }

    def reducer(x:Array[(Int,Int,Int)],y:Array[(Int,Int,Int)]):Array[(Int,Int,Int)]={
        var acc = Map[Int,(Int,Int)]()
        for(v<-(x++y)){
             if(acc.contains(v._1)){
                 val old = acc(v._1) 
                 acc.update(v._1,(v._2+old._1,v._3+old._2))
             }else{
                 acc.addOne((v._1,(v._2,v._3)))
             }
        }
        return acc.map((x)=>(x._1,x._2._1,x._2._2)).toArray

    }
    
}