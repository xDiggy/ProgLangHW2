Members: Queenie Sun (sunq6) and Antonio Martinez (martia20)

Features in Code:
- nodesListStringToListInt(Nodes)
    - Converts the list of strings to a list of integers

- edgesListStringToListInt(Edges)
    - Converts the list of strings to a list of integers

- doPartByAll(Data, GodMap)
    - Recursively parses the input file all at once and builds the graph data structures

- addAllColors(L1, L2)
    - Merges two lists of colors, removing duplicates

- addEdgesIntoNodes(GodMap)
    - Adds the edges to the nodes, creating a complete graph data structure

- goOverNodes(Nodes, Edges, NewNodeList)
    - Recursively traverses the graph and adds the edges to the nodes

- removeMirroredEdges(Edges)
    - Removes duplicate edges from the graph

- putUnique(Edges, NewList)
    - Recursively adds an edge to a list of edges, removing duplicates

- edgeInList(Edge, List)
    -  Checks if an edge is present in a list of edges

- countEdgesOfColor(Colors, Nodes, File)
    - Counts the number of edges of a given color in the graph and writes the results to the output file

- idekWhatToNameThisButEdges(Color, Nodes) 
    - Counts the number of edges of a given color in the graph for a given node

- idekWhatToNameThisButNodes(Color, Nodes)
    - Counts the number of nodes of a given color in the graph

- lengthOf(List) ->
    - Calculates the length of a list

- doPartByAllDriver(FilePath, A_out_file, B_out_file)
    - Parses the input file and partitions the graph into disjoint sets of nodes, then writes the number of edges of each color in the graph and the list of nodes with the maximum degree in each partition to the output files

- startPartB(Map, B_out_file)
    - Finds the nodes with the maximum degree in each partition and writes them to the output file

- findShitFromPartition(Parts, AllNodes, B_out_file)
    - Recursively searches through the partitions and finds the nodes with the maximum degree in each partition, then writes them to the output file

- findUID(IDS)
    - Removes duplicate nodes from a list of nodes

- printNodes(Nodes, B_out_file)
    -  Prints the nodes in a list to the output file

- edgesWithMaxDegree(Degree, Nodes) ->
    - Returns a list of the nodes in a list of nodes that have the maximum degree

- findMaxdegree(Tuples) ->
    - Returns the maximum degree from a list of tuples where the first element is the node ID and the second element is the degree

- uniqueNodes(Nodes) ->
    -  Removes duplicate nodes from a list of nodes

- findAllNodesFromEdges(Edges, AllNodes) ->
    - Returns a list of all the nodes in a list of edges

- findNodeFromID(ID, Nodes) ->
    - Returns the node with the given ID from a list of nodes

- findAllEdgesInPartition(Nodes) ->
    - Returns a list of all the edges in a partition

- nodeList(Nodes) ->
    - Returns a list of the node IDs in a list of nodes

- degreeOfNodes(Nodes, Return) ->
    - Recursively calculates the degree of each node in a list of nodes and returns a list of tuples where the first element is the node ID and the second element is the degree

- createListOfEdges(Input, Edges) ->
    - Recursively creates a list of edges from a list of lists where each sublist contains two node IDs

- createListOfNodes(Input, Nodes, Colors) ->
    -  Recursively creates a list of nodes from a list of lists where each sublist contains a node ID and a color

- edgesMatchingFrom(NodeID, Edges) ->
    - Returns a list of all the edges in a list of edges that match the given node ID

- addAll(List1, List2) ->
    - Merges two lists and removes duplicate elements

- parse_partitions(Input_file_path, A_out, B_out) ->
    - Parses the input file and partitions the graph into disjoint sets of nodes, then writes the number of edges of each color in the graph and the list of nodes with the maximum degree in each partition to the output files

- makePartitionsUseful(GodMap) ->
    - Converts the partitions in a map to a more useful format

- temp(Parts, Nodes, NewParts) ->
    - Recursively converts the partitions in a list to a more useful format

- partitionToListOfNodes(OldNodes, RealNodes, Return) ->
    - Recursively converts a partition to a list of nodes

- idToNode(Id, AllNodes) ->
    - Returns the node with the given ID from a map of nodes

- start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path)
    -  Parses the input file and partitions the graph, then writes the number of edges of each color in the graph and the list of nodes with the maximum degree in each partition to the output files


Bugs in Code:
- Did not develop the distributed version of the code