-module(graph_stats).

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
        [H|T] -> [list_to_integer(H) | nodesListStringToListInt(T)]
    end.

% convert list of strings to list of edge strings ("node,node") to list of lists of integers
edgesListStringToListInt(Edges) ->
    case Edges of
        [] -> [];
        [H|T] -> [ [list_to_integer(nth(1, tokens(H, ","))), list_to_integer(nth(2, tokens(H, ",")))] | edgesListStringToListInt(T)]
    end.

% parse inputs into correct data types
doPart(File) ->
    PartitionIDString = io:get_line(File, ''),
    PartitionID = list_to_integer(sub_string(PartitionIDString, 11, len(PartitionIDString)-1)),
    NodesTemp = strip(io:get_line(File, ''), right, $\n),
    Nodes = nodesListStringToListInt(tokens(NodesTemp, ",")),
    Colors = tokens(strip(io:get_line(File, ''), right, $\n), ","),
    EdgesTemp = strip(io:get_line(File, ''), right, $\n),
    Edges = edgesListStringToListInt(tokens(EdgesTemp, " ")),

    fwrite("Partition ID: ~p\n", [PartitionID]),
    fwrite("Nodes: ~p\n", [Nodes]),
    fwrite("Colors: ~p\n", [Colors]),
    fwrite("Edges: ~p\n", [Edges]).


parse_partitions(Input_file_path) ->
    {ok, File} = file:open(Input_file_path, [read]),
    doPart(File).


start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path) ->
    % code starts here
    io:fwrite("Hello, World!\n"),
    parse_partitions(Input_file_path).