-module(file_system).
-export([write_file/2, read_file/1, delete_file/1, create_dir/1, delete_dir/1, list_dir/1, rename_file/2]).

%% Creating a simple file system that can do CRUD operations
%% on files and directories.

%% Create a file
%% adding a guard so that only strings can be put in the File arg.
write_file(File, Data) when is_list(File) ->
    case file:open(File, [write]) of
        {ok, Fd} ->
            ok = file:write(Fd, Data),
            file:close(Fd),
            {ok, "File written successfully"};
        {error, Reason} ->
            io:format("Error writing to file: ~p~n", [Reason]),
            {error, Reason}
    end.

%% read a file and print to the terminal
read_file(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            io:format("File Contents:~n~s~n", [Data]),
            {ok, Data};
        {error, Reason} ->
            io:format("Error reading file: ~p~n", [Reason]),
            {error, Reason}
    end.

%%delete a file
delete_file(File) ->
    case file:delete(File) of
        ok ->
            io:format("File deleted successfully.~n"),
            {ok, "File deleted successfully"};
        {error, Reason} ->
            io:format("Error deleting file: ~p~n", [Reason]),
            {error, Reason}
    end.

%% create a directory
create_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            io:format("Directory created successfully.~n"),
            {ok, "Directory created successfully"};
        {error, Reason} ->
            io:format("Error creating directory: ~p~n", [Reason]),
            {error, Reason}
    end.

%% delete a directory
delete_dir(Dir) ->
    case file:del_dir(Dir) of
        ok ->
            io:format("Directory deleted successfully.~n"),
            {ok, "Directory deleted successfully"};
        {error, Reason} ->
            io:format("Error deleting directory: ~p~n", [Reason]),
            {error, Reason}
    end.

%%list the contents of a directory and print to the terminal
list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            io:format("Directory Contents:~n"),
            lists:foreach(fun(File) -> io:format("~s~n", [File]) end, Files),
            {ok, Files};
        {error, Reason} ->
            io:format("Error listing directory: ~p~n", [Reason]),
            {error, Reason}
    end.

%%Rename a file
rename_file(Old, New) ->
    case file:read_file_info(Old) of
        {ok, _Info} ->
            case file:rename(Old, New) of
                ok ->
                    io:format("File renamed successfully.~n"),
                    {ok, "File renamed successfully"};
                {error, Reason} ->
                    io:format("Error renaming file: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Error: file does not exist or is inaccessible - ~p~n", [Reason]),
            {error, "This file does not exist"}
    end.
