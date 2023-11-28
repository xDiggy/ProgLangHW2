% c("graph_stats"), graph_stats:start("../input.txt", "a_output.txt", "b_output.txt").

-module(graph_stats).

-import(io, [get_line/2]).      % for line from file
-import(file, [read_file/1]).   % for reading all from file
-import(file, [read/2]).        % for reading all from file
-import(io, [fwrite/1]).        % for printing
-import(io, [fwrite/2]).        % for printing
-import(string,[sub_string/3]). % for parsing - substring (string, startIdx, endIdx)
-import(string,[substr/3]).     % for parsing - substring (string, startIdx, length)
-import(string,[len/1]).        % length of string
-import(string,[tokens/2]).     % string to list from delimeter - tokens (string, delimiter)
-import(string,[strip/3]).      % strip whitespace from string - strip (string, left/right/both, char)
-import(string,[split/2]).      % split string into list of strings - split (string, delimiter)
-import(lists, [nth/2]).        % get nth element of list - nth (index, list)

-export([start/3]).

-record(edge, {from, to}).
-record(node, {id, color, edges}).
-record(partition, {id, nodes}).


% convert list of strings to list of integers
nodesListStringToListInt(Nodes) ->
    case Nodes of
        [] -> [];
        [H|T] ->
            {Int, _} = string:to_integer(H),
            [ Int | nodesListStringToListInt(T)]
    end.

% convert list of strings to list of edge strings ("node,node") to list of lists of integers
edgesListStringToListInt(Edges) ->
    case Edges of
        [] -> [];
        [H|T] ->
            Ints = tokens(H, ","),
            {Int1, _} = string:to_integer(nth(1, Ints)),
            {Int2, _} = string:to_integer(nth(2, Ints)),
            [[Int1, Int2] | edgesListStringToListInt(T)]
    end.

doPartByAll(Data, GodMap) ->
    case Data of
        [] -> GodMap;
        [IDX, NODESX, COLORSX, EDGESX|T] ->
            PartitionIDString = IDX,
            {PartitionID, _} = string:to_integer(sub_string(PartitionIDString, 11, len(PartitionIDString))),
            NodesTemp = strip(NODESX, right, $\n),
            Nodes = nodesListStringToListInt(tokens(NodesTemp, ",")),
            Colors = tokens(strip(COLORSX, right, $\n), ","),
            EdgesTemp = strip(EDGESX, right, $\n),
            Edges = edgesListStringToListInt(tokens(EdgesTemp, " ")),

            % fwrite("Partition ID: ~p\n", [PartitionID]),
            % fwrite("Nodes: ~w\n", [Nodes]),
            % fwrite("Colors: ~p\n", [Colors]),
            % fwrite("Edges: ~w\n", [Edges]),
            EdgeList = createListOfEdges(Edges, []),
            EdgesFromMap = maps:get("AllEdges", GodMap),
            LMAO = addAll(EdgesFromMap, EdgeList),
            NewMap = maps:put("AllEdges", [LMAO], GodMap),
            % fwrite("EdgeList: ~w\n", [EdgeList]),
            NodeList = createListOfNodes(Nodes, [], lists:reverse(Colors)),
            NodeListFromMap = maps:get("AllNodes", NewMap),
            LOL = addAll(NodeListFromMap, NodeList),
            NewNewMap = maps:put("AllNodes", [LOL], NewMap),
            % fwrite("NodeList: ~p\n", [NodeList]),
            % fwrite("LMAO: ~p\n", [LMAO]),
            % fwrite("LOL: ~p\n", [LOL]),

            X = #partition{id=PartitionID, nodes=NodeList},

            ExistingColors = maps:get("AllColors", NewNewMap),
            NewColors = addAllColors(Colors, ExistingColors),
            NewNewNewMap = maps:put("AllColors", NewColors, NewNewMap),
            OldPartitions = maps:get("AllPartitions", NewNewNewMap),
            NewNewNewNewMap = maps:put("AllPartitions", [X|OldPartitions], NewNewNewMap),
            % fwrite("---------------------\n"),
            % fwrite("FatBoi ~p\n", [NewNewNewNewMap]),
            % fwrite("NewColors: ~p\n", [NewColors]),

            doPartByAll(T, NewNewNewNewMap)

    end.

addAllColors(List1, List2) ->
    case List1 of
        [] -> List2;
        [H|T] ->
            CA = list_to_atom(H),
            case lists:member(CA, List2) of
                true -> addAllColors(T, List2);
                false ->
                    NewList = List2 ++ [CA],
                    addAllColors(T, NewList)
            end
    end.

addEdgesIntoNodes(GodMap) ->
    AllNodes = nth(1, maps:get("AllNodes", GodMap)),
    AlLEdges = maps:get("AllEdges", GodMap),
    NewNodeList = goOverNodes(AllNodes, AlLEdges, []),
    NewGodMap = maps:put("AllNodes", NewNodeList, GodMap),
    NewGodMap.

goOverNodes(Nodes, Edges, NewNodeList) ->
    case Nodes of
        [] -> NewNodeList;
        [H|T] ->
            NodeEdges = edgesMatchingFrom(H#node.id, Edges),
            NewNode = #node{id=H#node.id, color=H#node.color, edges=NodeEdges},
            goOverNodes(T, Edges, NewNodeList ++ [NewNode])
    end.

removeMirroredEdges(Edges) ->
    UniqueEdges = putUnique(nth(1,Edges), []),
    UniqueEdges.

putUnique(Edges, NewList) ->
    case Edges of
        [] -> NewList;
        [H|T] -> 
            X = H,
            NewEdges = T,
            Res = edgeInList(X, NewList),
            case Res of
                false -> putUnique(NewEdges, lists:merge([NewList, [X]]));
                true -> putUnique(NewEdges, NewList)
            end
    end.

edgeInList(Edge, List) ->
    case List of
        [] -> false;
        [H|T] ->
            Froms = Edge#edge.from == H#edge.from,
            Tos = Edge#edge.to == H#edge.to,
            NormalEqual = Froms and Tos,
            FromTo = Edge#edge.from == H#edge.to,
            ToFrom = Edge#edge.to == H#edge.from,
            FlippedEqual = FromTo and ToFrom,

            case (NormalEqual or FlippedEqual) of
                false -> edgeInList(Edge, T);
                true -> true
            end            
    end.

countEdgesOfColor(Colors, Nodes, File) ->
    case Colors of
        [] -> ok;
        [H|T] ->
            io:format(File, "~p, ", [H]),
            io:format(File, "~p, ", [idekWhatToNameThisButNodes(H, Nodes)]),
            case T of
                [] -> io:format(File, "~p", [idekWhatToNameThisButEdges(H, Nodes)]);
                _ -> io:format(File, "~p\n", [idekWhatToNameThisButEdges(H, Nodes)])
            end,
            countEdgesOfColor(T, Nodes, File)
    end.

idekWhatToNameThisButEdges(Color, Nodes) ->
    case Nodes of
        [] -> 0;
        [H|T] ->
            case H#node.color == Color of
                true -> lengthOf(H#node.edges) + idekWhatToNameThisButEdges(Color, T);
                false -> idekWhatToNameThisButEdges(Color, T)
            end
    end.

idekWhatToNameThisButNodes(Color, Nodes) ->
    case Nodes of
        [] -> 0;
        [H|T] ->
            case H#node.color == Color of
                true -> 1 + idekWhatToNameThisButNodes(Color, T);
                false -> idekWhatToNameThisButNodes(Color, T)
            end
    end.

lengthOf(List) ->
    case List of
        [] -> 0;
        [_|T] -> 1 + lengthOf(T)
    end.

doPartByAllDriver(FilePath, A_out_file, B_out_file) ->
    {ok, Dump} = read(FilePath, 1024*1024),
    Info = tokens(Dump, "\n"),
    GodMap = #{"AllNodes" => [], "AllEdges" => [], "AllColors" => [], "AllPartitions" => []},
    SOME = doPartByAll(Info, GodMap),
    Temp = removeMirroredEdges(maps:get("AllEdges", SOME)),
    AnotherMap = maps:put("AllEdges", Temp, SOME),
    DumbassMap = addEdgesIntoNodes(AnotherMap),
    AllColors = maps:get("AllColors", DumbassMap),
    AllNodes = maps:get("AllNodes", DumbassMap),

    {ok, A} = file:open(A_out_file, [write]),
    countEdgesOfColor(AllColors, AllNodes, A),

    BetterMap = makePartitionsUseful(DumbassMap),
    {ok, B} = file:open(B_out_file, [write]),
    startPartB(BetterMap, B).

startPartB(Map, B_out_file) -> 
    AllNodes = maps:get("AllNodes", Map),
    AllPartitions = maps:get("AllPartitions", Map),
    End = findShitFromPartition(lists:reverse(AllPartitions), AllNodes, B_out_file),
    End2 = lists:sort(findUID(End)),
    io:format(B_out_file, "G: ", []),
    printNodes(End2, B_out_file).

findShitFromPartition(Parts, AllNodes, B_out_file) -> 
    case Parts of
        [] -> [];
        [H|T] ->
            X = findAllEdgesInPartition(H#partition.nodes),
            AllEdgesInPart = removeMirroredEdges([X]),
            AllNodesFromAllEdges = findAllNodesFromEdges(AllEdgesInPart, AllNodes),
            UNodes = uniqueNodes(AllNodesFromAllEdges),
            AllNodeDegrees = degreeOfNodes(UNodes, []),
            MaxDegree = findMaxdegree(AllNodeDegrees),
            EdgesWithMaxDegree = edgesWithMaxDegree(MaxDegree, AllNodeDegrees),
            io:format(B_out_file, "partition ~p: ", [H#partition.id]),
            printNodes(lists:sort(EdgesWithMaxDegree), B_out_file),
            io:format(B_out_file, "\n", []),
            EdgesWithMaxDegree ++ findShitFromPartition(T, AllNodes, B_out_file)
    end.

findUID(IDS) ->
    case IDS of
        [] -> [];
        [H|T] ->
            case lists:member(H, T) of
                true -> findUID(T);
                false -> [H] ++ findUID(T)
            end
    end.

printNodes(Nodes, B_out_file) ->
    case Nodes of
        [] -> ok;
        [H|T] ->
            io:format(B_out_file, "~p", [H]),
            case T of
                [] -> ok;
                _ -> io:format(B_out_file, ",", [])
            end,
            printNodes(T, B_out_file)
    end.

edgesWithMaxDegree(Degree, Nodes) ->
    case Nodes of
        [] -> [];
        [H|T] ->
            case nth(2, H) == Degree of
                true -> [nth(1, H)] ++ edgesWithMaxDegree(Degree, T);
                false -> edgesWithMaxDegree(Degree, T)
            end
    end.

findMaxdegree(Tuples) ->
    case Tuples of
        [] -> 0;
        [H|T] ->
            X = findMaxdegree(T),
            case nth(2, H) > X of
                true -> nth(2, H);
                false -> X
            end
    end.

uniqueNodes(Nodes) ->
    case Nodes of
        [] -> [];
        [H|T] ->
            case lists:member(H#node.id, nodeList(T)) of
                true -> uniqueNodes(T);
                false -> [H] ++ uniqueNodes(T)
            end
    end.

findAllNodesFromEdges(Edges, AllNodes) ->
    case Edges of
        [] -> [];
        [H|T] ->
            From = H#edge.from,
            To = H#edge.to,
            FromNode = findNodeFromID(From, AllNodes),
            ToNode = findNodeFromID(To, AllNodes),
            [FromNode, ToNode] ++ findAllNodesFromEdges(T, AllNodes)
    end.

findNodeFromID(ID, Nodes) ->
    case Nodes of
        [] -> [];
        [H|T] ->
            case H#node.id == ID of
                true -> H;
                false -> findNodeFromID(ID, T)
            end
    end.

findAllEdgesInPartition(Nodes) ->
    case Nodes of
        [] -> [];
        [H|T] ->
            AllEdges = H#node.edges,
            AllEdges ++ findAllEdgesInPartition(T)
    end.

nodeList(Nodes) ->
    case Nodes of
        [] -> [];
        [H|T] ->
            [H#node.id] ++ nodeList(T)
    end.

degreeOfNodes(Nodes, Return) ->
    case Nodes of
        [] -> Return;
        [H|T] ->
            NewReturn = [[H#node.id, lengthOf(H#node.edges)]] ++ Return,
            NR = degreeOfNodes(T, NewReturn),
            NR
    end.

createListOfEdges(Input, Edges) ->
    case Input of
        [] -> Edges;
        [[H|T]|O] ->
            NewEdge = #edge{from=H, to=nth(1, T)},
            NewList = [NewEdge|Edges],
            createListOfEdges(O, NewList)
    end.

createListOfNodes(Input, Nodes, Colors) ->
    case Input of
        [] -> Nodes;
        [H|T] ->
            NewNode = #node{id=H, color=list_to_atom(lists:last(Colors)), edges=[]},
            NewList = [NewNode|Nodes],
            createListOfNodes(T, NewList, lists:droplast(Colors))
    end.

edgesMatchingFrom(NodeID, Edges) ->
    case Edges of
        [] -> [];
        [H|T] ->
            Flag1 = H#edge.from == NodeID,
            Flag2 = H#edge.to == NodeID,
            case Flag1 or Flag2 of
                true -> [H|edgesMatchingFrom(NodeID, T)];
                false -> edgesMatchingFrom(NodeID, T)
            end
    end.

addAll(List1, List2) ->
    case List1 of
        [] -> List2;
        [H|T] ->
            NewList = lists:merge([List2, H]),
            addAll(T, NewList)
    end.

parse_partitions(Input_file_path, A_out, B_out) ->
    {ok, File} = file:open(Input_file_path, [read]),
    doPartByAllDriver(File, A_out, B_out).

makePartitionsUseful(GodMap) ->
    Partitions = maps:get("AllPartitions", GodMap),
    Nodes = maps:get("AllNodes", GodMap),
    SomeBS = temp(Partitions, Nodes, []),
    BetterMap = maps:put("AllPartitions", SomeBS, GodMap),
    BetterMap.

temp(Parts, Nodes, NewParts) ->
    case Parts of
        [] -> NewParts;
        [H|T] ->
            NewNodes = partitionToListOfNodes(H#partition.nodes, Nodes, []),
            NewPartition = #partition{id=H#partition.id, nodes=NewNodes},
            temp(T, Nodes, NewParts ++ [NewPartition])
    end.

partitionToListOfNodes(OldNodes, RealNodes, Return) ->
    case OldNodes of
        [] -> Return;
        [H|T] ->
            NodeID = H#node.id,
            Node = idToNode(NodeID, RealNodes),
            NewReturn = [Node] ++ Return,
            Again = partitionToListOfNodes(T, RealNodes, NewReturn),
            Again
    end.

idToNode(Id, AllNodes) ->
    case AllNodes of
        [] -> [];
        [H|T] ->
            case H#node.id == Id of
                true -> H;
                false -> idToNode(Id, T)
            end
    end.


start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path) ->
    parse_partitions(Input_file_path, Part_a_output_file_path, Part_b_output_file_path).