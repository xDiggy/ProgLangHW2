-module(graph_stats).

-import(io, [fwrite/2]).
-import(string,[sub_string/3]).
-import(string,[substr/3]).
-import(string,[len/1]).
-import(string,[tokens/2]).
-import(string,[strip/3]).
-import(string,[split/2]).

-export([start/3]).

-record(edge, {from, to}).
-record(node, {id, color, edges}).
-record(partition, {id, nodes}).

% write temp function that takes in a string, nodes, of ints
% seperated by a comma, and return a list of integers
listStringToListInt(Nodes) ->
    case Nodes of
        [] -> [];
        [H|T] -> [list_to_integer(H) | listStringToListInt(T)]
    end.


doPart(File) ->
    PartitionIDString = io:get_line(File, ''),
    PartitionID = list_to_integer(sub_string(PartitionIDString, 11, len(PartitionIDString)-1)),
    Nodes = strip(io:get_line(File, ''), right, $\n),
    Colors = strip(io:get_line(File, ''), right, $\n),
    Edges = strip(io:get_line(File, ''), right, $\n),

    NewNodes = listStringToListInt(tokens(Nodes, ",")),

    fwrite("Partition ID: ~p\n", [PartitionID]),
    fwrite("Nodes: ~p\n", [tokens(Nodes, ",")]),
    fwrite("Colors: ~p\n", [Colors]),
    fwrite("Edges: ~p\n", [Edges]),

    fwrite("Temp: ~p\n", [NewNodes]).


parse_partitions(Input_file_path) ->
    {ok, File} = file:open(Input_file_path, [read]),
    doPart(File).


start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path) ->
    % code starts here
    io:fwrite("Hello, World!\n"),
    parse_partitions(Input_file_path).