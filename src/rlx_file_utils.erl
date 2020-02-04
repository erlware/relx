-module(rlx_file_utils).

-export([mkdir_p/1,
         wildcard_paths/1,
         symlink_or_copy/2]).

-include_lib("kernel/include/file.hrl").

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_p(string()) -> ok | {error, Reason::file:posix()}.
mkdir_p(Path) ->
    %% We are exploiting a feature of ensuredir that that creates all
    %% directories up to the last element in the filename, then ignores
    %% that last element. This way we ensure that the dir is created
    %% and not have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).

%% @doc expand wildcards and names in the given paths
-spec wildcard_paths([file:filename_all()]) -> [string()].
wildcard_paths(Paths) ->
    [filename:absname(Expanded) || Path <- Paths, Expanded <- wildcard(Path)].

%% In case the given directory does not expand,
%% we return it back in a list so we trigger the
%% proper error reportings.
-spec wildcard(file:filename_all()) -> [string()].
wildcard(Path) when is_binary(Path) ->
    wildcard(binary_to_list(Path));
wildcard(Path) when is_list(Path) ->
    case filelib:wildcard(Path) of
        []   -> [Path];
        Paths -> Paths
    end.

symlink_or_copy(Source, Target) ->
    case file:make_symlink(Source, Target) of
        ok ->
            ok;
        {error, eexist} ->
            {error, eexist};
        {error, Err} ->
            case {os:type(), Err} of
                {{win32, _}, eperm} ->
                    % We get eperm on Windows if we do not have
                    %   SeCreateSymbolicLinkPrivilege
                    % Try the next alternative
                    win32_make_junction_or_copy(Source, Target);
                _ ->
                    % On other systems we try to copy next
                    cp_r(Source, Target)
            end
    end.

cp_r(Source, Target) ->
    copy(Source, Target, [{recursive, true}, {file_info, [mode, time, owner, group]}]).

win32_make_junction_or_copy(Source, Target) ->
    case filelib:is_dir(Source) of
        true ->
            win32_make_junction(Source, Target);
        _ ->
            cp_r(Source, Target)
    end.

win32_make_junction(Source, Target) ->
    % The mklink will fail if the target already exists, check for that first
    case file:read_link_info(Target) of
        {error, enoent} ->
            win32_make_junction_cmd(Source, Target);
        {ok, #file_info{type = symlink}} ->
            case file:read_link(Target) of
                {ok, Source} ->
                    ok;
                {ok, _} ->
                    ok = file:del_dir(Target),
                    win32_make_junction_cmd(Source, Target);
                {error, Reason} ->
                    {error, {readlink, Reason}}
            end;
        {ok, #file_info{type = _Type}} ->
            % Directory already exists, so we overwrite the copy
            cp_r(Source, Target);
        Error ->
            Error
    end.

win32_make_junction_cmd(Source, Target) ->
    S = unicode:characters_to_list(Source),
    T = unicode:characters_to_list(Target),
    Cmd = "cmd /c mklink /j " ++ filename:nativename(T) ++ " " ++ filename:nativename(S),
    case os:cmd(Cmd) of
        "Junction created " ++ _ ->
            ok;
        [] ->
            % When mklink fails it prints the error message to stderr which
            % is not picked up by os:cmd() hence this case switch is for
            % an empty message
            cp_r(Source, Target)
    end.

%% @doc copy an entire directory to another location.
copy(From, To, Options) ->
    case proplists:get_value(recursive,  Options, false) of
        true ->
            case is_dir(From) of
                false ->
                    copy_(From, To, Options);
                true ->
                    make_dir_if_dir(To),
                    copy_subfiles(From, To, Options)
            end;
        false ->
            copy_(From, To, Options)
    end.

copy_(From, To, Options) ->
    case file:copy(From, To) of
        {ok, _} ->
            copy_file_info(To, From, proplists:get_value(file_info, Options, []));
        {error, Error} ->
            {error, {copy_failed, Error}}
    end.

copy_file_info(To, From, FileInfoToKeep) ->
    case file:read_file_info(From) of
        {ok, FileInfo} ->
            case write_file_info(To, FileInfo, FileInfoToKeep) of
                [] ->
                    ok;
                Errors ->
                    {error, {write_file_info_failed_for, Errors}}
            end;
        {error, RFError} ->
            {error, {read_file_info_failed, RFError}}
    end.

write_file_info(To, FileInfo, FileInfoToKeep) ->
    WriteInfoFuns = [{mode, fun try_write_mode/2},
                     {time, fun try_write_time/2},
                     {group, fun try_write_group/2},
                     {owner, fun try_write_owner/2}],
    lists:foldl(fun(Info, Acc) ->
                        case proplists:get_value(Info, WriteInfoFuns, undefined) of
                            undefined ->
                                Acc;
                            F ->
                                case F(To, FileInfo) of
                                    ok ->
                                        Acc;
                                    {error, Reason} ->
                                        [{Info, Reason} | Acc]
                                end
                        end
                end, [], FileInfoToKeep).


try_write_mode(To, #file_info{mode=Mode}) ->
    file:write_file_info(To, #file_info{mode=Mode}).

try_write_time(To, #file_info{atime=Atime, mtime=Mtime}) ->
    file:write_file_info(To, #file_info{atime=Atime, mtime=Mtime}).

try_write_owner(To, #file_info{uid=OwnerId}) ->
    file:write_file_info(To, #file_info{uid=OwnerId}).

try_write_group(To, #file_info{gid=OwnerId}) ->
    file:write_file_info(To, #file_info{gid=OwnerId}).

is_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            true;
        _ ->
            false
    end.

-spec make_dir_if_dir(file:name()) -> ok | {error, term()}.
make_dir_if_dir(File) ->
    case is_dir(File) of
        true  -> ok;
        false -> mkdir_path(File)
    end.

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_path(file:name()) -> ok | {error, Reason::term()}.
mkdir_path(Path) ->
    mkdir_p(Path).

copy_subfiles(From, To, Options) ->
    Fun =
        fun(ChildFrom) ->
                ChildTo = filename:join([To, filename:basename(ChildFrom)]),
                copy(ChildFrom, ChildTo, Options)
        end,
    lists:foreach(Fun, sub_files(From)).

sub_files(From) ->
    {ok, SubFiles} = file:list_dir(From),
    [filename:join(From, SubFile) || SubFile <- SubFiles].
