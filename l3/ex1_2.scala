import scala.collection.mutable.Map
import io.Source


object Graph{
    
    def main(args: Array[String]) = {
        val result = Source.fromFile("web-Stanford.txt").getLines.map(mapper).reduce(reducer)
        println()
        for(v<-result){
             print(v._1)
             print(" ")
             print(v._2._1)
             print(" ")
             print(v._2._2)
             print(" ")
             println()
        }
        
    }

    def mapper(arg:String):Map[Int,(Int,Int)]={
        val nodes = arg.split("\t").map((x)=>x.toInt).toArray
        val map = Map[Int,(Int,Int)]()
        val v0 = nodes(0)
        val v1 = nodes(1)
        map.addOne((v0,(0,1)))
        
        if(map.contains(v1)){
            val old = map(v1)
            map.update(v1,(1,1))
        }else{
            map.addOne((v1,(1,0)))
        }
        return map
    }

    def reducer(x:Map[Int,(Int,Int)],y:Map[Int,(Int,Int)]):Map[Int,(Int,Int)]={
        
        for(v<-y.toArray){
            if(x.contains(v._1)){
                val old = x(v._1)
                x.update(v._1,(old._1+v._2._1,old._2+v._2._2))
            }else{
                x.addOne(v)
            }
        }
        return x

    }
    
}