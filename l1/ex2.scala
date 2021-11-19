import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
object MapReduce{
	def main(args: Array[String]) = {
		var input = List((1, List(2,3)), (3, List(1, 5)), (2, List(5)), (5, List()))
		print(input)
		println()
		val mapping_output = mapping(input)
		print(mapping_output)
		println()
		val reduce_output = reduce(mapping_output)
		print(reduce_output)
	}

	def mapping(input:List[(Int,List[Int])]):ListBuffer[(Int,Int)] = {
		var mapping  = ListBuffer[(Int,Int)]()
		for(a<-input){
			for(b<-a._2){
				mapping+=((b,a._1))
			}
		}
		return mapping
	}

	def reduce(input:ListBuffer[(Int,Int)]):List[(Int,List[Int])]={
		var m = Map[Int,ListBuffer[Int]]()
		for(x<-input){
			if(m.contains(x._1)){
				m(x._1)=(m(x._1)+=x._2)
			}else{
				m+=(x._1->ListBuffer(x._2))
			}
		}
		return m.toList.map(x=>(x._1,x._2.toList))
	}
}
