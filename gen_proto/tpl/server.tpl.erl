%%----------------------------------------------------
%% {{description}} 
%% 该文件由程序生成，不要手动修改
%%----------------------------------------------------
-module({{mod_name}}).
-export([
        pack/3
        ,unpack/3
    ]
).{{include_file}}


-define(TO_BIN(S), case is_list(S) of
        true -> unicode:characters_to_binary(S);
        _ -> S
    end).
%%----------------------------------------------------
%% @doc 打包命令
%%----------------------------------------------------
-spec pack(srv | cli, non_neg_integer(), tuple()) ->
    {ok, binary()} | {error, {unknown_pack_command, non_neg_integer()}}.{{pack_fun}}

pack(_Type, Code, _Data) ->
    {error, {unknown_pack_command, Code}}.

%%----------------------------------------------------
%% @doc 解包命令
%%----------------------------------------------------
-spec unpack(srv | cli, non_neg_integer(), binary()) ->
    {ok, tuple()} | {error, {unknown_unpack_command, non_neg_integer()}}.{{unpack_fun}}

unpack(_Type, Code, _Data) ->
    {error, {unknown_unpack_command, Code}}.
