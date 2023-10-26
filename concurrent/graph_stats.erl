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

% parse inputs into correct data types
doPartByLine(File) ->
    PartitionIDString = strip(get_line(File, ''), right, $\n),
    {PartitionID, _} = string:to_integer(sub_string(PartitionIDString, 11, len(PartitionIDString))),
    NodesTemp = strip(io:get_line(File, ''), right, $\n),
    Nodes = nodesListStringToListInt(tokens(NodesTemp, ",")),
    Colors = tokens(strip(io:get_line(File, ''), right, $\n), ","),
    EdgesTemp = strip(io:get_line(File, ''), right, $\n),
    Edges = edgesListStringToListInt(tokens(EdgesTemp, " ")),

    fwrite("Partition ID: ~p\n", [PartitionID]),
    fwrite("Nodes: ~w\n", [Nodes]),
    fwrite("Colors: ~p\n", [Colors]),
    fwrite("Edges: ~w\n", [Edges]).


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

            fwrite("Partition ID: ~p\n", [PartitionID]),
            fwrite("Nodes: ~w\n", [Nodes]),
            fwrite("Colors: ~p\n", [Colors]),
            fwrite("Edges: ~w\n", [Edges]),
            EdgeList = createListOfEdges(Edges, []),
            EdgesFromMap = maps:get("AllEdges", GodMap),
            LMAO = addAll(EdgesFromMap, EdgeList),
            NewMap = maps:put("AllEdges", [LMAO], GodMap),
            fwrite("EdgeList: ~w\n", [EdgeList]),
            NodeList = createListOfNodes(Nodes, [], lists:reverse(Colors)),
            NodeListFromMap = maps:get("AllNodes", NewMap),
            LOL = addAll(NodeListFromMap, NodeList),
            NewNewMap = maps:put("AllNodes", [LOL], NewMap),
            fwrite("NodeList: ~p\n", [NodeList]),
            fwrite("LMAO: ~p\n", [LMAO]),
            fwrite("LOL: ~p\n", [LOL]),

            ExistingColors = maps:get("AllColors", NewNewMap),
            NewColors = addAllColors(Colors, ExistingColors),
            NewNewNewMap = maps:put("AllColors", NewColors, NewNewMap),
            fwrite("NewColors: ~p\n", [NewColors]),

            doPartByAll(T, NewNewNewMap)

    end.

addColors(Existing, Colors) ->
    case Colors of
        [] -> Existing;
        [H|T] ->
            ColorAtom = list_to_atom(H),
            case lists:member(ColorAtom, Existing) of
                true -> addColors(Existing, T);
                false ->
                    fwrite("Adding color: ~p\n", [ColorAtom]),
                    fwrite("Existing: ~p\n", [Existing]),
                    Temp = addAll([ColorAtom], Existing),
                    fwrite("Temp: ~p\n", [Temp]),
                    addColors(Temp, T)
                % false -> addColors(lists:merge([H, Existing]), T)
            end
    end.

addAllColors(List1, List2) ->
    case List1 of
        [] -> List2;
        [H|T] ->
            CA = list_to_atom(H),
            case lists:member(CA, List2) of
                true -> addAllColors(T, List2);
                false ->
                    fwrite("Adding color: ~p\n", [CA]),
                    fwrite("Existing: ~p\n", [List2]),
                    NewList = List2 ++ [CA],
                    fwrite("NewList: ~p\n", [NewList]),
                    addAllColors(T, NewList)
            end
    end.

addEdgesIntoNodes(GodMap) ->
    % XNodes = nth(1, maps:get("AllNodes", GodMap)),
    % LastNode = lists:last(XNodes),
    % YNodes = lists:droplast(XNodes),
    % fwrite("YNodes: ~p\n", [YNodes]),
    % fwrite("LastNode: ~p\n", [LastNode]).
    fwrite("nah gang u got other shit to worry abt\n").

removeMirroredEdges(Edges) ->
    % fwrite("**~p**\n", [nth(1,Edges)]),
    UniqueEdges = putUnique(nth(1,Edges), []).

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

doPartByAllDriver(FilePath) ->
    {ok, Dump} = read(FilePath, 1024*1024),
    fwrite("Dump: ~p\n", [Dump]),
    Info = tokens(Dump, "\n"),
    fwrite("Info: ~p\n", [Info]),
    GodMap = #{"AllNodes" => [], "AllEdges" => [], "AllColors" => [], "AllPartitions" => []},
    SOME = doPartByAll(Info, GodMap),
    fwrite("--------- AFTER PARSE ---------\n"),
    fwrite("Some: ~p\n", [SOME]),
    fwrite("--------- DOING NODE TINGS ---------\n"),
    removeMirroredEdges(maps:get("AllEdges", SOME)),
    addEdgesIntoNodes(SOME).


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
            case H#edge.from == NodeID of
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

parse_partitions(Input_file_path) ->
    {ok, File} = file:open(Input_file_path, [read]),
    % doPartByLine(File).
    doPartByAllDriver(File).

start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path) ->
    % code starts here
    fwrite("Starting Program graph_stats\n"),
    fwrite("---------------------\n"),
    parse_partitions(Input_file_path).