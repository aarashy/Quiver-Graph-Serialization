/**
  * Created by Aarash Heydari on 3/3/2017
  *
  * This is a naive interface for serializing and deserializing graphs using Quiver.
  * The next step is to use an exterior JSON scala library to be able to recall type information.
  *
  * Note that currently, this interface has the unfortunate side effect of mapping toString onto
  * all fields of the graph when recreating.
  */
package quiver

object GraphSerializer {
  //This main method demos what I've done.
  def main(args: Array[String]): Unit = {

    val nil = empty[String,String,String]

    val a = nil & Context(Vector(), "Joe", "The Murderer", Vector())
    val b = a & Context(Vector("Kills" -> "Joe", "is_named" -> "Bobby"), "Bobby", "The King", Vector())
    val c = b.addNode(LNode("Johnzo", "The Jester"))
    val d = c.addEdge(LEdge("Bobby", "Johnzo", "is_friends_with"))
    val s = SerializeGraph(d)

    System.out.println("Here's the output of the default serializer:")
    System.out.println(s)

    System.out.println("Here's the output of serializing by context:")
    val k = SerializeByContext(c)
    System.out.println(k)

    //System.out.println(GraphDeserializer.DeserializeGraph(k)) not yet implemented
    System.out.println(d)
    System.out.println(GraphDeserializer.DeserializeGraph(s))
  }

  /**
    * Serializes a Graph by listing the nodes, then the edges.
    * @param graph A graph to be serialized
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @tparam B Type of the Labels of Edges
    * @return The serialized Graph as a String
    */
  def SerializeGraph[N, A, B](graph: Graph[N, A, B]): String = {
    //DefaultSerializer(graph)
    "{\n" + SerializeNodeVector(graph.labNodes) + SerializeEdgeVector(graph.labEdges) + "\n}"
  }
  /**
    *  Format a Labelled Node into a string.
    * @param n A node to be serialized
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @return The toString() of each field of the Node.
    */
  def FormatNode[N,A](n: LNode[N, A]): String = {
    "\t\"Vertex\": " + n.vertex.toString()  +
      ",\n\t\"Label\": " + n.label.toString
  }

  /**
    * Calls FormatNode to serialize a Labelled Node.
    * @param n A node to be serialized
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @return The serialized Labelled Node.
    */
  def SerializeNode[N, A](n: LNode[N,A]): String = {
    "{\n" + FormatNode(n) + "\n}"
  }

  /**
    * Serializes a vector of Labelled Nodes one after another, separated by newlines.
    * @param vec A vector of nodes to be serialized
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @return The serialized Labelled Nodes separated by newlines.
    */
  def SerializeNodeVector[N, A](vec: Vector[LNode[N,A]]): String = {
    "\"Nodes\": [\n" + vec.foldLeft("")((total, next) => total + SerializeNode(next) + "\n") + "]\n"
  }

  /**
    * Format a Labelled Edge into a string.
    * @param e An edge to be serialized
    * @tparam N Type of the Vertices
    * @tparam B Type of the Edge's Label
    * @return The toString() of each field of the edge.
    */
  def FormatEdge[N,B](e: LEdge[N, B]): String = {
    "\t\"From\": " + e.from.toString() + ",\n\t\"To\": " + e.to.toString() +
      ",\n\t\"Label\": " + e.label.toString()
  }

  /**
    * Calls the FormatEdge method to serialize an edge
    * @param e An edge to be serialized
    * @tparam N Type of the Vertices
    * @tparam B Type of the Edge's Label
    * @return The serialized Edge
    */
  def SerializeEdge[N, B](e: LEdge[N,B]): String = {
    "{\n" + FormatEdge(e) + "\n}"
  }

  /**
    * Serializes a vector of Labelled Edges one after another, separated by newlines.
    * @param vec A vector of nodes to be serialized
    * @tparam N Type of the Vertices
    * @tparam B Type of the Edge's Label
    * @return The serialized Labelled Edges separated by newlines.
    */
  def SerializeEdgeVector[N, B](vec: Vector[LEdge[N,B]]): String = {
    "\"Edges\": [\n" + vec.foldLeft("")((total, next) => total + SerializeEdge(next) + "\n") + "]"
  }

  /**
    * Serializes a graph by repeatedly applying decompAny and serializing each Context.
    * This method is not advised for use; I haven't finished deserializing by Context.
    * @param graph A graph to be serialized
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @tparam B Type of the Labels of Edges
    * @return The Graph serialized by Context
    */
  def SerializeByContext[N, A, B](graph: Graph[N, A, B]): String = {
    if(graph.isEmpty) ""
    else ContextSerialHelper(graph, Vector())
  }

  /**
    * Helper Method for Serializing by Context.
    * Decomposes the graph into a Vector of Contexts which can embedded together to recreate the graph.
    * Calls SerializeContextVector on that Vector.
    * @param graph A graph being decomposed into a Vector of Contexts
    * @param vec A growing Vector of Contexts
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @tparam B Type of the Labels of Edges
    * @return The Serialized Context Vector associated with the original graph.
    */
  def ContextSerialHelper[N, A, B](graph: Graph[N, A, B], vec: Vector[Context[N,A,B]]): String = {
    if (graph.isEmpty) return SerializeContextVector(vec)
    val decom = graph.decompAny
    ContextSerialHelper[N, A, B](decom.rest, vec.+:(decom.ctx.get))
  }
  /**
    * Format a Context into a String.
    * @param context A Context to be serialized
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @tparam B Type of the Labels of Edges
    * @return The toString() of four fields of the context.
    *         Perhaps this ought
    */
  def FormatContext[N,A,B](context: Context[N,A,B]): String = {
    "\t\"Predecessors\": " + context.predecessors.toString() + ",\n\t\"Vertex\": " + context.vertex.toString()  +
      ",\n\t\"Label\": " + context.label  + ",\n\t\"Successors\": " + context.successors.toString()
  }

  /**
    * Calls FormatContext to serialize a Context.
    * @param context A Context to be serialized
    * @tparam N Type of the Vertex
    * @tparam A Type of the Labels of Nodes
    * @tparam B Type of the Labels of Edges
    * @return The serialized Context.
    */
  def SerializeContext[N, A, B](context: Context[N,A,B]): String = {
    "\"Context\": {\n" + FormatContext(context) + "\n}"
  }

  /**
    * Serializes a vector of Contexts one after another, separating each with one newline.
    * @param vec A vector of Contexts to be serialized
    * @tparam N Type of the Vertices
    * @tparam A Type of the Labels of Nodes
    * @tparam B Type of the Labels of Edges
    * @return The serialized Contexts separated by newlines.
    */
  def SerializeContextVector[N, A, B](vec: Vector[Context[N,A,B]]): String = {
    vec.foldLeft("")((total, next) => total + SerializeContext(next) + "\n")
  }

}
