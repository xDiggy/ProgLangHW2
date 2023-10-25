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



doPartByAllDriver(FilePath) ->
    {ok, Dump} = read(FilePath, 1024*1024),
    fwrite("Demp: ~p\n", [Dump]),
    Info = tokens(Dump, "\n"),
    fwrite("Info: ~p\n", [Info]),
    PartMap = #{"default" => none},
    doPartByAll(Info, PartMap).


doPartByAll(Data, PartMap) ->
    case Data of
        [] -> ok;
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
            fwrite("EdgeList: ~w\n", [EdgeList]),
            NodeList = createListOfNodes(Nodes, [], lists:reverse(Colors)),
            fwrite("NodeList: ~p\n", [NodeList]),
            doPartByAll(T, PartMap)
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
            NewNode = #node{id=H, color=lists:last(Colors), edges=[]},
            NewList = [NewNode|Nodes],
            createListOfNodes(T, NewList, lists:droplast(Colors))
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