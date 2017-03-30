/**
  * Created by Aarash Heydari on 3/3/2017
  *
  * This is an object for Deserializing the strings produced by my GraphSerializer object's
  * SerializeGraph method.
  */
package quiver

object GraphDeserializer {
  /**
    * Deserializes a serialized graph in the format produced by my GraphSerializer object's
    * SerializeGraph method. By design, has the unfortunate side effect of mapping toString
    * onto all fields of the graph.
    * 
    * Deserializing by context has not yet been implemented.
    *
    * @param str A serialized graph outputted by the SerializeGraph method.
    * @return Either successfully returns the original Graph which was serialized, with
    *         toString mapped to every field, or prints an error message and returns
    *         an empty graph.
    */
  def DeserializeGraph(str: String): Graph[String,String,String] = {
    val s = str.split("[\n]+").map(_.trim())

    /**
      * A recursive helper method that curries nodeList and edgeList as it parses the serialized graph.
      * Either successfully returns the list of nodes and the list of edges,
      * or returns None.
      * @param index The line number of the string being parsed.
      * @param nodeList Begins as empty and is curried along during the execution of the function.
      * @param edgeList Same as above
      * @param mode Keeps track of what is currently being parsed. 1 indicates nodes are being parsed; 2 indicates edges, and 3, contexts.
      * @return
      */
    def DeserializeHelper(index: Int=0, nodeList: Seq[LNode[String,String]] = Seq(), edgeList: Seq[LEdge[String,String]] = Seq(), mode: Int = 0)
    : Option[(Seq[LNode[String,String]], Seq[LEdge[String,String]])] = {
      val p = s(index)
      if (mode == 0) {
        p match {
          case "{" => DeserializeHelper(index + 1, nodeList, edgeList)
          case "\"Nodes\": [" => DeserializeHelper(index + 1, nodeList, edgeList, 1)
          case "\"Edges\": [" => DeserializeHelper(index + 1, nodeList, edgeList, 2)
          case "\"Contexts\":" => DeserializeHelper(index + 1, nodeList, edgeList, 3)
          case "}" => Some((nodeList, edgeList))
          case _ => None
        }
      }
      else if (mode == 1) {
        p match {
          case "[" => DeserializeHelper(index + 1, nodeList, edgeList, 1)
          case "{" =>
            val v = s(index+1).substring(10, s(index+1).length()-1)
            val l = s(index+2).substring(9)
            val n = LNode(v, l)
            return DeserializeHelper(index+3, nodeList=nodeList.+:(n), edgeList, mode=1)
          case "}" => DeserializeHelper(index + 1, nodeList, edgeList, mode=1)
          case "]" => DeserializeHelper(index + 1, nodeList, edgeList)
          case _ => None
        }
      }
      else if (mode == 2) p match {
        case "[" => DeserializeHelper(index + 1, nodeList, edgeList, mode=2)
        case "{" =>
          val f = s(index+1).substring(8, s(index+1).length()-1)
          val t = s(index+2).substring(6, s(index+2).length()-1)
          val l = s(index+3).substring(9)
          val n = LEdge(f, t, l)
          return DeserializeHelper(index+4, nodeList, edgeList=edgeList.+:(n), mode=2)
        case "}" => DeserializeHelper(index + 1, nodeList, edgeList, mode=2)
        case "]" => DeserializeHelper(index + 1, nodeList, edgeList)
        case _ => None
      }
      /* Here is where I would handle deserializing by context;
      however, this functionality is not yet added.
      else if (mode == 3) p match {
        case "[" => DeserializeHelper(index + 1, nodeList, edgeList, mode=3)
        case "{" =>
          val f = s(index+1).substring(8, s(index+1).length()-1)
          val t = s(index+2).substring(6, s(index+2).length()-1)
          val l = s(index+3).substring(9)
          val n = LEdge(f, t, l)
          return DeserializeHelper(index+4, nodeList, edgeList=edgeList.+:(n), mode=3)
        case "}" => DeserializeHelper(index + 1, nodeList, edgeList, mode=2)
        case "]" => DeserializeHelper(index + 1, nodeList, edgeList)
        case _ => None
      }
      */
      else None
    }
    try {
      val res = DeserializeHelper().get
      return empty[String, String, String].addNodes(res._1.reverse).addEdges(res._2.reverse)
    }
    catch {
      case e: Exception => System.out.println("Parsing Error")
        empty[String,String,String]
    }
  }


}
