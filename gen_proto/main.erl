%%----------------------------------------------------
%% 协议生成工具
%%----------------------------------------------------
-module(main).
-export([
        compile/1
    ]
).
-include("common.hrl").
-include("protocol.hrl").
-include("condition.hrl").
-define(SRV_PATH, "../../server/src/proto/").

compile(all) ->
    ?P("---------------------------------~n"),
    ?P("> 正在生成协议...~n"),
    ?P("---------------------------------~n"),
    List = cfg_file(),
    put(rpc, []),
    put(desc_fun, []),
    all(List),
    gen_rpc_cfg(),
    ?P("> 所有协议已生成~n"),
    ok;
compile(Mod) when is_list(Mod) ->
    compile(list_to_atom(Mod));
compile(Mod) ->
    Info = Mod:info(),
    L = Mod:cfg(),
    %% 打包客户端
    CmdHead = get_lua_cmd_head(L),
    gen_lua_packs(CmdHead, L),
    %% 打包服务端
    gen_srv_proto(Mod, Info, L),
    rpc_cfg(L),
    ok.

%% 处理所有命令
all([]) -> ok;
all([H | T]) ->
    compile(H),
    all(T).

gen_srv_proto(Mod, {Description, IncFiles}, L) ->
    Inc = ["\n-include(\"" ++ X ++ "\")." || X <- IncFiles],
    Pack = lists:concat([gen_erl_pack(cli, Code, ReqFields) ++ gen_erl_pack(srv, Code, ReplyFields) || #rpc{code = Code, req = ReqFields, reply = ReplyFields} <- L]),
    Unpack = lists:concat([gen_erl_unpack(srv, Code, ReqFields) ++ gen_erl_unpack(cli, Code, ReplyFields) || #rpc{code = Code, req = ReqFields, reply = ReplyFields} <- L]),
    Vars = [
        {description, Description}
        ,{include_file, Inc}
        ,{mod_name, Mod}
        ,{pack_fun, Pack}
        ,{unpack_fun, Unpack}
    ],
    Text = util:template("./tpl/server.tpl.erl", Vars),
    File = lists:concat([?SRV_PATH, Mod, ".erl"]),
    case file:write_file(File, unicode:characters_to_binary(Text)) of
        ok -> ?P("> ~s.erl~n", [Mod]);
        Err -> ?P("生成协议[~s]时发生异常: ~w~n", [File, Err])
    end.

%% 保存rpc_cfg内容
gen_rpc_cfg() ->
    Vars = [{rpc, lists:reverse(get(rpc))}, {desc_fun, lists:reverse(get(desc_fun))}],
    Text = util:template("./tpl/rpc_cfg.tpl.erl", Vars),
    case file:write_file(lists:concat([?SRV_PATH, "rpc_cfg.erl"]), unicode:characters_to_binary(Text)) of
        ok -> ?P("> rpc_cfg.erl~n");
        Err -> ?P("生成rpc_cfg.erl时发生异常: ~w~n", [Err])
    end.

%% 生成rpc配置内容
rpc_cfg([]) -> ok;
rpc_cfg([H | T]) ->
    {R, D} = do_rpc_cfg(H),
    put(rpc, [R | get(rpc)]),
    put(desc_fun, [D | get(desc_fun)]),
    rpc_cfg(T).
do_rpc_cfg(Rpc = #rpc{code = Code, req = ReqFields, req_desc = ReqDesc, reply = ReplyFields, reply_desc = ReplyDesc}) ->
    put(code, Code),
    R = Rpc#rpc{
        req_desc = {rpc_cfg, desc_fun, {req, Code}}
        ,req = undefined
        ,reply_desc = {rpc_cfg, desc_fun, {reply, Code}}
        ,reply = undefined
    },
    S = lists:concat(["get(", Code, ") ->\n", s(1), util:to_string(R), ";\n\n"]),
    F1 = lists:concat(["desc_fun({req, ", Code, "}, _Data)-> ", cdesc(ReqDesc, ReqFields), ";\n"]),
    F2 = lists:concat(["desc_fun({reply, ", Code, "}, _Data)-> ", cdesc(ReplyDesc, ReplyFields), ";\n"]),
    {S, F1 ++ F2}.

%% record_to_string(L, Rec) ->
%%     S = r2s(L, []),
%%     lists:concat(["#", Rec, "{", S, "}"]).
%% 
%% r2s([], Str) -> t(lists:concat(lists:reverse(Str)));
%% r2s([{K, V} | T], Str) ->
%%     S = lists:concat([K, " = ", V, ", "]),
%%     r2s(T, [S | Str]).

%% 生成日志格式化代码
cdesc(undefined, _) -> "\"\"";
cdesc({Desc, Vars}, Fields) ->
    V = vals(Vars, Fields, []),
    lists:concat(["io_lib:format(\"", Desc, "\", [", V, "])"]);
cdesc(Desc, _) ->
    lists:concat(["\"", Desc, "\""]).

%% 生成取值代码
vals([], _F, L) -> string:join(lists:reverse(L), ", ");
vals([N | T], F, L) ->
    P = pos(F, 1, N),
    vals(T, F, [?S("element(~w, _Data)", [P]) | L]).

%% 定位某项在list中的位置
pos([], _P, N) ->
    ?ERR("[Code: ~w]的desc项配置有误，没有找到对应的项[~ts]", [get(code), N]),
    exit(error);
pos([{_, N, _} | _T], P, N) -> P;
pos([{_, _, N, _, _} | _T], P, N) -> P;
pos([_ | T], P, N) -> pos(T, P + 1, N).

%% 生成打包函数
gen_erl_pack(Type, Code, []) ->
    lists:concat(["\n\npack(", Type, ", ", Code, ", {}) ->\n", s(1), "{ok, <<2:32, ", Code, ":16>>};"]);
gen_erl_pack(Type, Code, {RecordName, Fields}) ->
    put(code, Code),
    Vars = lists:concat(["#", RecordName, "{", vr(Fields, 1), "}"]),
    Txt = erl_pack(Fields, 1, []),
    lists:concat(["\n\npack(", Type, ", ", Code, ", ", Vars, ") ->\n", s(1), "D_a_t_a = ", Txt, ",\n", s(1),"{ok, <<(byte_size(D_a_t_a) + 2):32, ", Code, ":16, D_a_t_a/binary>>};"]);
gen_erl_pack(Type, Code, Fields) ->
    put(code, Code),
    Txt = erl_pack(Fields, 0, []),
    Vars = "{" ++ v(Fields, 0) ++ "}",
    lists:concat(["\n\npack(", Type, ", ", Code, ", ", Vars, ") ->\n", s(1), "D_a_t_a = ", Txt, ",\n", s(1),"{ok, <<(byte_size(D_a_t_a) + 2):32, ", Code, ":16, D_a_t_a/binary>>};"]).

%% 处理record项
erl_pack([], _Deep, Vars) ->
    V = t(lists:concat(lists:reverse(Vars))),
    "<<" ++ V ++ ">>";
erl_pack([{int8, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ":8/signed, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{uint8, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ":8, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{int16, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ":16/signed, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{uint16, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ":16, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{int32, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ":32/signed, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{uint32, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ":32, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{float, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, "/float, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{string, N, _} | T], Deep, Vars) ->
    X = lists:concat(["V", Deep, "_", N]),
    V = lists:concat(["(byte_size(?TO_BIN(", X, "))):16, (?TO_BIN(", X, "))/binary, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{byte, N, _} | T], Deep, Vars) ->
    X = lists:concat(["V", Deep, "_", N]),
    V = lists:concat(["(byte_size(", X, ")):32, ", X, "/binary, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{tuple, N, Fields} | T], Deep, Vars) ->
    %% @wpf 增加单个tuple元素的解析方法
    A = t(erl_pack(tuple_fields(N, Fields), Deep, [])),
    A1 = lists:reverse(t(lists:reverse(A))),
    A2 = lists:concat([A1, ", "]),
    erl_pack(T, Deep, [A2 | Vars]);
erl_pack([{rec, _N, _RecN, Fields} | T], Deep, Vars) ->
    %% erl_pack(Fields ++ T, Deep, Vars);
    A = t(erl_pack(Fields, Deep*10, [])), %% TODO 区分rec下的字段变量
    A1 = lists:reverse(t(lists:reverse(A))),
    A2 = lists:concat([A1, ", "]),
    erl_pack(T, Deep, [A2 | Vars]);
erl_pack([{array, tuple, N, _, Fields} | T], Deep, Vars) ->
    S = "{" ++ v(Fields, Deep + 1) ++ "}",
    X = lists:concat(["V", Deep, "_", N]),
    A = erl_pack(Fields, Deep + 1, []),
    V = lists:concat(["(length(", X, ")):16, (list_to_binary([", A, " || ", S, " <- ", X, "]))/binary, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{array, list, N, _, Fields} | T], Deep, Vars) ->
    S = "[" ++ v(Fields, Deep + 1) ++ "]",
    X = lists:concat(["V", Deep, "_", N]),
    A = erl_pack(Fields, Deep + 1, []),
    V = lists:concat(["(length(", X, ")):16, (list_to_binary([", A, " || ", S, " <- ", X, "]))/binary, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{array, single, N, _, Fields} | T], Deep, Vars) ->
    S = v(Fields, Deep + 1),
    X = lists:concat(["V", Deep, "_", N]),
    A = erl_pack(Fields, Deep + 1, []),
    V = lists:concat(["(length(", X, ")):16, (list_to_binary([", A, " || ", S, " <- ", X, "]))/binary, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{array, RecordName, N, _, Fields} | T], Deep, Vars) ->
    S = lists:concat(["#", RecordName, "{", vr(Fields, Deep + 1), "}"]),
    X = lists:concat(["V", Deep, "_", N]),
    A = erl_pack(Fields, Deep + 1, []),
    V = lists:concat(["(length(", X, ")):16, (list_to_binary([", A, " || ", S, " <- ", X, "]))/binary, "]),
    erl_pack(T, Deep, [V | Vars]);
erl_pack([{_, N, _} | _T], _Deep, _Vars) ->
    ?ERR("[code: ~w]的[~w]项配置有误", [get(code), N]),
    exit(error);
erl_pack([{_, _, N, _, _} | _T], _Deep, _Vars) ->
    ?ERR("[code: ~w]的[~w]项配置有误", [get(code), N]),
    exit(error).

%% 获取变量名字串
v(Fields, Deep) ->
    v(Fields, Deep, []).
v([], _Deep, Vars) -> t(lists:concat(lists:reverse(Vars)));
v([{tuple, N, Fields} | T], Deep, Vars) ->
    V = "{" ++ v(tuple_fields(N, Fields), Deep) ++ "}, ",
    v(T, Deep, [V | Vars]);
v([{_, N, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ", "]),
    v(T, Deep, [V | Vars]);
v([{array, _, N, _, _} | T], Deep, Vars) ->
    V = lists:concat(["V", Deep, "_", N, ", "]),
    v(T, Deep, [V | Vars]);
v([{rec, _N, RecN, Fields} | T], Deep, Vars) ->
    V = lists:concat(["#", RecN, "{", vr(Fields, Deep), "}, "]),
    v(T, Deep, [V | Vars]).

tuple_fields(N, Fields) ->
    [{Tx, lists:concat([N ,"_", Vx]), Dx} || {Tx,Vx,Dx} <- Fields].
%% rec_fields(N, Fields) ->
%%     ?DEBUG("N:~w Fields:~w", [N, Fields]),
%%     lists:map(fun({tuple, Nx, Fs}) ->
%%                 {tuple, lists:concat([N ,"_", Nx]), tuple_fields(lists:concat([N ,"_", Nx]), Fs)};
%%             ({Tx, Nx, Dx}) ->
%%                 {Tx, lists:concat([N ,"_", Nx]), Dx};
%%             ({array, Tx, Nx, Dx, Mx}) ->
%%                 {array, Tx, lists:concat([N ,"_", Nx]), Dx, Mx};
%%             (_F) ->
%%                 ?ERR("不支持的配置:~w", [_F]),
%%                 exit(error)
%%         end, Fields).

%% 获取记录名字串
vr(Fields, Deep) ->
    vr(Fields, Deep, []).
vr([], _Deep, Vars) -> t(lists:concat(lists:reverse(Vars)));
vr([{tuple, N, L} | T], Deep, Vars) ->
    %% @wpf 增加记录中的单个元素是tuple类型的数据解析
    V = lists:concat([N, " = {", v(tuple_fields(N, L), Deep), "}, "]),
    vr(T, Deep, [V | Vars]);
vr([{_, N, _} | T], Deep, Vars) ->
    V = lists:concat([N, " = V", Deep, "_", N, ", "]),
    vr(T, Deep, [V | Vars]);
vr([{array, _, N, _, _} | T], Deep, Vars) ->
    V = lists:concat([N, " = V", Deep, "_", N, ", "]),
    vr(T, Deep, [V | Vars]);
vr([{rec, N, RecN, Fields} | T], Deep, Vars) ->
    V = lists:concat([N , " = #", RecN, "{", vr(Fields, Deep*10), "}, "]),  %% TODO 区分rec下的字段变量
    vr(T, Deep, [V | Vars]).

%% 生成解包函数
gen_erl_unpack(Type, Code, []) ->
    lists:concat(["\n\nunpack(", Type, ", ", Code, ", _B0) ->\n", s(1), "{ok, {}};"]);
gen_erl_unpack(Type, Code, {_RecordName, Fields}) ->
    gen_erl_unpack(Type, Code, Fields);
gen_erl_unpack(Type, Code, Fields) ->
    {_Idx, _Deep, Txt} = erl_unpack(Fields, 0, 1, [], []),
    lists:concat(["\n\nunpack(", Type, ", ", Code, ", _B0) ->\n", Txt, ";"]).
erl_unpack([], Idx, Deep, Vars, Str) ->
    V = case Deep =:= 1 of
        true ->
            lists:concat([s(1), "{ok, {", t(lists:concat([N ++ ", " || N <- lists:reverse(Vars)])), "}}"]);
        false ->
            lists:concat([s(Deep), "{{", t(lists:concat([N ++ ", " || N <- lists:reverse(Vars)])), "}, _B", Idx, "}"])
    end,
    S = lists:concat(lists:reverse(Str)),
    {Idx + 1, Deep, S ++ V};
erl_unpack([{array, _Type, Name, _Desc, Fields} | T], Idx, Deep, Vars, Str) ->
    V = lists:concat(["V", Deep, "_", Name]),
    X = s(Deep),
    {Idx1, Deep1, Txt} = erl_unpack(Fields, Idx + 1, Deep + 1, [], []),
    S = lists:concat([X, "{", V, ", _B", Idx1 + 1, "} = protocol:array(_B", Idx, ", fun(_B", Idx +1, ") ->\n", Txt, "\n", X, "end),\n"]),
    erl_unpack(T, Idx1 + 1, Deep1 - 1, [V | Vars], [S | Str]);
erl_unpack([{tuple, Name, Fields} | T], Idx, Deep, Vars, Str) ->
    %% @wpf 增加记录中的单个元素是tuple类型的数据解析
    {NewIdx, NewV, NewS} = erl_unpack_tuple(tuple_fields(Name, Fields), Idx, Deep, Vars, Str),
    erl_unpack(T, NewIdx, Deep, NewV, NewS);
erl_unpack([{Type, Name, _Desc} | T], Idx, Deep, Vars, Str) ->
    V = lists:concat(["V", Deep, "_", Name]),
    X = s(Deep),
    S = lists:concat([X, "{", V, ", _B", Idx + 1, "} = protocol:", Type, "(_B", Idx, "),\n"]),
    erl_unpack(T, Idx + 1, Deep, [V | Vars], [S | Str]);
erl_unpack([{rec, _N, _RecN, Fields} | T], Idx, Deep, Vars, Str) ->
    erl_unpack(Fields ++ T, Idx, Deep, Vars, Str).
%% 添加的tuple组合
%% @wpf 增加记录中的单个元素是tuple类型的数据解析
erl_unpack_tuple([], Idx, _Deep, Vars, Str) ->
    {Idx, Vars, Str};
erl_unpack_tuple([{array, _Type, Name, _Desc, Fields} | T], Idx, Deep, Vars, Str) ->
    V = lists:concat(["V", Deep, "_", Name]),
    X = s(Deep),
    {Idx1, Deep1, Txt} = erl_unpack_tuple(Fields, Idx + 1, Deep + 1, [], []),
    S = lists:concat([X, "{", V, ", _B", Idx1 + 1, "} = protocol:array(_B", Idx, ", fun(_B", Idx +1, ") ->\n", Txt, "\n", X, "end),\n"]),
    erl_unpack_tuple(T, Idx1 + 1, Deep1 - 1, [V | Vars], [S | Str]);
erl_unpack_tuple([{Type, Name, _Desc} | T], Idx, Deep, Vars, Str) ->
    V = lists:concat(["V", Deep, "_", Name]),
    X = s(Deep),
    S = lists:concat([X, "{", V, ", _B", Idx + 1, "} = protocol:", Type, "(_B", Idx, "),\n"]),
    erl_unpack_tuple(T, Idx + 1, Deep, [V | Vars], [S | Str]).


%% ===============================
%% 生成Lua打包命令
%% ===============================
get_lua_cmd_head([#rpc{code = Code}|_]) -> trunc(Code / 100);
get_lua_cmd_head(_L) ->
    ?P("~w", [_L]),
    exit({error, can_not_get_lua_cmd_head}).

gen_lua_packs(CmdHead, L) ->
    gen_lua_packs(CmdHead, L, "", "").
gen_lua_packs(CmdHead, [], ContentPack, ContentUnPack) ->
    Vars = [
        {content_pack, ContentPack}
        ,{content_unpack, ContentUnPack}
    ],
    Text = util:template("./tpl/client_common.tpl.lua", Vars),
    FileName = lists:concat(["./cli/P", CmdHead, ".lua"]),
    case file:write_file(FileName, Text) of
        ok ->
            ok;
        Err ->
            ?P("生成~ts时发生异常: ~w~n", [FileName, Err]),
            exit(error)
    end;
gen_lua_packs(CmdHead, [Rpc|T], ContentPack, ContentUnPack) ->
    Text1 = gen_lua_pack(Rpc),
    ContentPack1 = lists:concat([ContentPack, "\n", Text1]),
    Text2 = gen_lua_unpack(Rpc),
    ContentUnPack1 = lists:concat([ContentUnPack, "\n", Text2]),
    gen_lua_packs(CmdHead, T, ContentPack1, ContentUnPack1).

gen_lua_pack(#rpc{code = Code, req = Fields}) ->
    put(code, Code),
    ClassName = lists:concat(["P", Code]),
    {ok, MainClass, SubClasses} = gen_lua_pack_main(Fields, ClassName, [], [], ""),
    Vars = [
        {main_class, MainClass}
        ,{sub_classes, SubClasses}
    ],
    Text = util:template("./tpl/client_pack.tpl.lua", Vars),
    Text.

%% 对于主类的类成员打包 -> {ok, MainClassStr :: string(), SubClassesStr :: string()}
gen_lua_pack_main([], ClassName, MemberList, FillList, SubClassesStr) ->
    Members = to_lua_str(lists:reverse(MemberList)),
    Fills = to_lua_str(lists:reverse(FillList)),
    Vars = [
        {class, ClassName}
        ,{members, Members}
        ,{fills, Fills}
    ],
    MainClassStr = util:template("./tpl/client_pack_main_class.tpl.lua", Vars),
    {ok, MainClassStr, SubClassesStr};
gen_lua_pack_main([Field|T], ClassName, MemberList, FillList, SubClassesStr) ->
    case lua_pack_field(Field, ClassName) of
        {ok, MemberStr, FillStr} ->
            gen_lua_pack_main(T, ClassName, [MemberStr|MemberList], [FillStr|FillList], SubClassesStr);
        {ok, MemberStr, FillStr, SubStr1} ->
            SubStr2 = lists:concat([SubClassesStr, SubStr1]),
            gen_lua_pack_main(T, ClassName, [MemberStr|MemberList], [FillStr|FillList], SubStr2)
    end.

%% 对于主类下面的类成员打包
gen_lua_pack_sub([], _ClassName, SubClassName, MemberList, FillList, SubClassesStr) ->
    Members = to_lua_str(lists:reverse(MemberList)),
    Fills = to_lua_str(lists:reverse(FillList)),
    Vars = [
        {class, SubClassName}
        ,{members, Members}
        ,{fills, Fills}
    ],
    Text = util:template("./tpl/client_pack_class.tpl.lua", Vars),
    Text1 = lists:concat([Text, SubClassesStr]),
    {ok, Text1};
gen_lua_pack_sub([Field|T], ClassName, SubClassName, MemberList, FillList, SubClassesStr) ->
    case lua_pack_field(Field, ClassName) of
        {ok, MemberStr, FillStr} ->
            gen_lua_pack_sub(T, ClassName, SubClassName, [MemberStr|MemberList], [FillStr|FillList], SubClassesStr);
        {ok, MemberStr, FillStr, SubStr1} ->
            SubStr2 = lists:concat([SubClassesStr, SubStr1]),
            gen_lua_pack_sub(T, ClassName, SubClassName, [MemberStr|MemberList], [FillStr|FillList], SubStr2)
    end.

%% 对每个域成员进行打包
lua_pack_field({array, single, FieldName, _Desc, FieldMembers}, _ClassName) ->
    case length(FieldMembers) =/=1 of
        true ->
            ?P("数组成员类型为single，必须只能定义一个成员", []),
            exit(error);
        false ->
            MemberStr = lists:concat(["    self.", FieldName, " = {}\n"]),
            [{SubFieldType, _SubFieldName, _SubFieldDesc}] = FieldMembers,
            FuncName = type2wfunc(SubFieldType),
            FillStr = lists:concat(["    _ba:writeUShort(#self.", FieldName, ")\n",
                "    for _,", FieldName, "_item in ipairs(self.", FieldName, ") do\n",
                "       _ba:", FuncName, "(", FieldName, "_item)\n",
                "    end\n"]),
            {ok, MemberStr, FillStr}
    end;
lua_pack_field({array, _MemberType, FieldName, _Desc, MemberList}, ClassName) ->
    MemberStr = lists:concat(["    self.", FieldName, " = {}\n"]),
    FillStr = lists:concat(["    _ba:writeUShort(#self.", FieldName, ")\n",
            "    for _,", FieldName, "_item in ipairs(self.", FieldName, ") do\n",
            "        ", FieldName, "_item:fill(_ba)\n",
            "    end\n"]),
    SubClassName = lists:concat([ClassName, "_", FieldName, "_item"]),
    {ok, SubStr} = gen_lua_pack_sub(MemberList, ClassName, SubClassName, [], [], "\n"),
    {ok, MemberStr, FillStr, SubStr};
lua_pack_field({FieldType, FieldName, _Desc}, _ClassName) ->
    do_lua_pack_field(FieldName, type2default(FieldType), type2wfunc(FieldType));
lua_pack_field(_Other, _ClassName) ->
    ?P("Unsupported type:~w when packing", [_Other]),
    exit(error).

%% 对单个域进行处理
do_lua_pack_field(FieldName, DefaultVal, FuncName) ->
    MemberStr = lists:concat(["    self.", FieldName, " = ", DefaultVal, "\n"]),
    FillStr = lists:concat(["    _ba:", FuncName, "(self.", FieldName, ")\n"]),
    {ok, MemberStr, FillStr}.


%% ===============================
%% 生成Lua解包命令
%% ===============================
%% gen_lua_unpacks(CmdHead, L) ->
%%     gen_lua_unpacks(CmdHead, L, "").
%% gen_lua_unpacks(CmdHead, [], Content) ->
%%     Vars = [
%%         {content, Content}
%%     ],
%%     Text = util:template("./tpl/client_common.tpl.lua", Vars),
%%     FileName = lists:concat(["./cli/U", CmdHead, ".lua"]),
%%     case file:write_file(FileName, Text) of
%%         ok ->
%%             ok;
%%         Err ->
%%             ?P("生成~ts时发生异常: ~w~n", [FileName, Err]),
%%             exit(error)
%%     end;
%% gen_lua_unpacks(CmdHead, [Rpc|T], Content) ->
%%     Text = gen_lua_unpack(Rpc),
%%     Content1 = lists:concat([Content, "\n", Text]),
%%     gen_lua_unpacks(CmdHead, T, Content1).

gen_lua_unpack(#rpc{code = Code, reply = Fields}) ->
    put(code, Code),
    ClassName = lists:concat(["U", Code]),
    {ok, MainClass, SubClasses} = gen_lua_unpack_main(Fields, ClassName, [], [], ""),
    Vars = [
        {main_class, MainClass}
        ,{sub_classes, SubClasses}
    ],
    Text = util:template("./tpl/client_unpack.tpl.lua", Vars),
    Text.

%% 对于主类的类成员解包 -> {ok, MainClassStr :: string(), SubClassesStr :: string()}
gen_lua_unpack_main({_Rec, [Field|T]}, ClassName, MemberList, FillList, SubClassesStr) ->
    gen_lua_unpack_main([Field|T], ClassName, MemberList, FillList, SubClassesStr);
gen_lua_unpack_main([], ClassName, MemberList, FillList, SubClassesStr) ->
    Members = to_lua_str(lists:reverse(MemberList)),
    Fills = to_lua_str(lists:reverse(FillList)),
    Vars = [
        {class, ClassName}
        ,{members, Members}
        ,{fills, Fills}
    ],
    MainClassStr = util:template("./tpl/client_unpack_main_class.tpl.lua", Vars),
    {ok, MainClassStr, SubClassesStr};
gen_lua_unpack_main([Field|T], ClassName, MemberList, FillList, SubClassesStr) ->
    case lua_unpack_field(Field, ClassName) of
        {ok, MemberStr, FillStr} ->
            gen_lua_unpack_main(T, ClassName, [MemberStr|MemberList], [FillStr|FillList], SubClassesStr);
        {ok, MemberStr, FillStr, SubStr1} ->
            SubStr2 = lists:concat([SubClassesStr, SubStr1]),
            gen_lua_unpack_main(T, ClassName, [MemberStr|MemberList], [FillStr|FillList], SubStr2)
    end.

%% 对于主类下面的类成员打包
gen_lua_unpack_sub([], _ClassName, SubClassName, MemberList, FillList, SubClassesStr) ->
    Members = to_lua_str(lists:reverse(MemberList)),
    Fills = to_lua_str(lists:reverse(FillList)),
    Vars = [
        {class, SubClassName}
        ,{members, Members}
        ,{fills, Fills}
    ],
    Text = util:template("./tpl/client_unpack_class.tpl.lua", Vars),
    Text1 = lists:concat([Text, SubClassesStr]),
    {ok, Text1};
gen_lua_unpack_sub([Field|T], ClassName, SubClassName, MemberList, FillList, SubClassesStr) ->
    case lua_unpack_field(Field, ClassName) of
        {ok, MemberStr, FillStr} ->
            gen_lua_unpack_sub(T, ClassName, SubClassName, [MemberStr|MemberList], [FillStr|FillList], SubClassesStr);
        {ok, MemberStr, FillStr, SubStr1} ->
            SubStr2 = lists:concat([SubClassesStr, SubStr1]),
            gen_lua_unpack_sub(T, ClassName, SubClassName, [MemberStr|MemberList], [FillStr|FillList], SubStr2)
    end.

%% 对每个域成员进行打包
lua_unpack_field({array, single, FieldName, _Desc, FieldMembers}, _ClassName) ->
    case length(FieldMembers) =/=1 of
        true ->
            ?P("数组成员类型为single，必须只能定义一个成员", []),
            exit(error);
        false ->
            MemberStr = lists:concat(["    self.", FieldName, " = {}\n"]),
            [{SubFieldType, _SubFieldName, _SubFieldDesc}] = FieldMembers,
            FuncName = type2rfunc(SubFieldType),
            FillStr = lists:concat(["    local ", FieldName, "_len = _ba:readUShort()\n",
                "    for i=1,", FieldName, "_len do\n",
                "        local v = _ba:", FuncName, "()\n",
                "        table.insert(self.", FieldName, ", v)\n"
                "    end\n"]),
            {ok, MemberStr, FillStr}
    end;
lua_unpack_field({array, _MemberType, {FieldName, SubArrayName}, _Desc, MemberList}, ClassName) ->
    MemberStr = lists:concat(["    self.", FieldName, " = {}\n"]),
    FillStr = lists:concat(["    local ", SubArrayName, "_len = _ba:readUShort()\n",
            "    for i=1,", SubArrayName, "_len do\n",
            "        local v = ", ClassName, "_", SubArrayName, "_item.new()\n",
            "        v:fill(_ba)\n",
            "        table.insert(self.", FieldName, ", v)\n"
            "    end\n"]),
    SubClassName = lists:concat([ClassName, "_", SubArrayName, "_item"]),
    {ok, SubStr} = gen_lua_unpack_sub(MemberList, ClassName, SubClassName, [], [], "\n"),
    {ok, MemberStr, FillStr, SubStr};
lua_unpack_field({array, _MemberType, FieldName, _Desc, MemberList}, ClassName) ->
    lua_unpack_field({array, _MemberType, {FieldName, FieldName}, _Desc, MemberList}, ClassName);
lua_unpack_field({tuple, FieldName, SubFields}, ClassName) ->
    TableHead = lists:concat(["    self.", FieldName, " = {}\n"]),
    {ok, MemberStr, FillStr} = lua_unpack_tuple(SubFields, FieldName, ClassName, TableHead, ""),
    {ok, MemberStr, FillStr};
lua_unpack_field({rec, FieldName, _RecName, SubFields}, ClassName) ->
    TableHead = lists:concat(["    self.", FieldName, " = {}\n"]),
    lua_unpack_rec(SubFields, FieldName, ClassName, TableHead, "", "");
lua_unpack_field({FieldType, FieldName, _Desc}, _ClassName) ->
    do_lua_unpack_field(FieldName, type2default(FieldType), type2rfunc(FieldType));
lua_unpack_field(_Other, _ClassName) ->
    ?P("Unsupported type:~w when unpacking", [_Other]),
    exit(error).

%% 对tuple进行处理
lua_unpack_tuple([], _SuperFieldName, _ClassName, MemberStr, FillStr) -> {ok, MemberStr, FillStr};
lua_unpack_tuple([{FieldType, FieldName, Desc}|T], SuperFieldName, ClassName, MemberStr, FillStr) ->
    {ok, Ms, Fs} = lua_unpack_field({FieldType, lists:concat([SuperFieldName, ".", FieldName]), Desc}, ClassName),
    MemberStr1 = lists:concat([MemberStr, Ms]),
    FillStr1 = lists:concat([FillStr, Fs]),
    lua_unpack_tuple(T, SuperFieldName, ClassName, MemberStr1, FillStr1).

%% 对rec进行处理（展开）
lua_unpack_rec([], _SuperFieldName, _ClassName, MemberStr, FillStr, SubStr) -> {ok, MemberStr, FillStr, SubStr};
lua_unpack_rec([{FieldType, FieldName, RecName, SubFields}|T], SuperFieldName, ClassName, MemberStr, FillStr, SubStr) ->
    Name1 = case SuperFieldName of
        [] -> FieldName;
        _ -> lists:concat([SuperFieldName, ".", FieldName])
    end,
    {ok, Ms, Fs, Ss} = lua_unpack_field({FieldType, Name1, RecName, SubFields}, ClassName),
    MemberStr1 = lists:concat([MemberStr, Ms]),
    FillStr1 = lists:concat([FillStr, Fs]),
    SubStr1 = lists:concat([SubStr, Ss]),
    lua_unpack_rec(T, SuperFieldName, ClassName, MemberStr1, FillStr1, SubStr1);
lua_unpack_rec([{FieldType, FieldName, Desc}|T], SuperFieldName, ClassName, MemberStr, FillStr, SubStr) ->
    Name1 = case SuperFieldName of
        [] -> FieldName;
        _ -> lists:concat([SuperFieldName, ".", FieldName])
    end,
    {ok, Ms, Fs} = lua_unpack_field({FieldType, Name1, Desc}, ClassName),
    MemberStr1 = lists:concat([MemberStr, Ms]),
    FillStr1 = lists:concat([FillStr, Fs]),
    lua_unpack_rec(T, SuperFieldName, ClassName, MemberStr1, FillStr1, SubStr);
lua_unpack_rec([{array, FieldType, FieldName, Desc, MemberList}|T], SuperFieldName, ClassName, MemberStr, FillStr, SubStr) ->
    {SubFieldName, SubArrayName} = case SuperFieldName of
        [] -> {FieldName, FieldName};
        _ -> {
                lists:concat([SuperFieldName, ".", FieldName])
                ,re:replace(lists:concat([SuperFieldName, "_", FieldName]), "\\.", "_", [{return, list}, global])
            }
    end,
    case lua_unpack_field({array, FieldType, {SubFieldName, SubArrayName}, Desc, MemberList}, ClassName) of
    %% case lua_unpack_field({array, FieldType, FieldName, Desc, MemberList}, ClassName) of
        {ok, Ms, Fs} ->
            MemberStr1 = lists:concat([MemberStr, Ms]),
            FillStr1 = lists:concat([FillStr, Fs]),
            lua_unpack_rec(T, SuperFieldName, ClassName, MemberStr1, FillStr1, SubStr);
        {ok, Ms, Fs, SubS} ->
            MemberStr1 = lists:concat([MemberStr, Ms]),
            FillStr1 = lists:concat([FillStr, Fs]),
            lua_unpack_rec(T, SuperFieldName, ClassName, MemberStr1, FillStr1, [SubS | SubStr])
    end.

%% 对单个域进行处理
do_lua_unpack_field(FieldName, DefaultVal, FuncName) ->
    MemberStr = lists:concat(["    self.", FieldName, " = ", DefaultVal, "\n"]),
    FillStr = lists:concat(["    self.", FieldName, " = _ba:", FuncName, "()\n"]),
    {ok, MemberStr, FillStr}.


%% 域类型 转成 对应的写方法名
type2wfunc(int8) -> "writeByte";
type2wfunc(uint8) -> "writeUByte";
type2wfunc(int16) -> "writeShort";
type2wfunc(uint16) -> "writeUShort";
type2wfunc(int32) -> "writeInt";
type2wfunc(uint32) -> "writeUInt";
type2wfunc(string) -> "writeStringUShort".

%% 域类型 转成 对应的读方法名
type2rfunc(int8) -> "readByte";
type2rfunc(uint8) -> "readUByte";
type2rfunc(int16) -> "readShort";
type2rfunc(uint16) -> "readUShort";
type2rfunc(int32) -> "readInt";
type2rfunc(uint32) -> "readUInt";
type2rfunc(string) -> "readStringUShort".

%% 域类型 转成 对应的默认值
type2default(int8) -> 0;
type2default(uint8) -> 0;
type2default(int16) -> 0;
type2default(uint16) -> 0;
type2default(int32) -> 0;
type2default(uint32) -> 0;
type2default(string) -> "\"\"";
type2default(tuple) -> nil.

%% 列表转成字符串 -> string()
to_lua_str([]) -> "";
to_lua_str(L) ->
    to_lua_str(L, "").
to_lua_str([], S) -> S;
to_lua_str([Str|T], S) ->
    S1 = lists:concat([S, Str]),
    to_lua_str(T, S1).



%% 缩进处理
s(N) -> s(N, "").
s(0, S) -> S;
s(N, S) -> s(N - 1, S ++ "    ").

%% 尾部处理
t(S) ->
    [_ | [_ | R]] = lists:reverse(S),
    lists:reverse(R).

%% 获取所有的配置文件
cfg_file() ->
    %% ?DEBUG("配置目录:~ts", [env:get(code_path) ++ "/src/proto"]),
    case file:list_dir("./cfg") of
        {ok, L} -> do_cfg_file(L, []);
        {error, Why} ->
            ?P("获取RPC配置文件时发生异常: ~w~n", [Why]),
            []
    end.

%% 返回所有的配置文件
do_cfg_file([], List) -> List;
do_cfg_file([F | T], List) ->
    L = case filename:extension(F) =:= ".erl" of
        true ->
            M = filename:basename(filename:rootname(F)),
            [M | List];
        _ -> List
    end,
    do_cfg_file(T, L).
