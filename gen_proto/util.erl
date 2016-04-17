%%----------------------------------------------------
%% 工具包
%%----------------------------------------------------
-module(util).
-export(
    [
        sleep/1
        ,is_process_alive/1
        ,bool2int/1
        ,for/3
        ,md5/1
        ,hmac_sha1/2
        ,floor/1
        ,check_range/3
        ,check_max/2
        ,check_min/2
        ,ceil/1
        ,readlines/1
        ,load/1
        ,save/2
        ,template/2
        ,replace/3
        ,rand/2
        ,rand_list/1
        ,rand_list/2
        ,tuplelist_to_record_string/1
        ,fbin/2
        ,bjoin/2
        ,cn/1
        ,cn/2
        ,build_fun/1
        ,to_string/1
        ,to_list/1
        ,to_atom/1
        ,parse_qs/1
        ,parse_qs/3
        ,term_to_string/1
        ,string_to_term/1
        ,term_to_bitstring/1
        ,all_to_binary/1
        ,to_binary/1
        ,check_name/3
        ,text_filter/1
        ,text_filter/2
        ,text_banned/1
        ,check_text/2
        ,check_text/3
        ,time_left/2
        ,utf8_len/1
        ,sub_big_list/2
        ,one_to_list/2
        ,get_stacktrace/0
        ,close_dets/1
        ,remove_duplicate/1
        ,pb_to_binary/1
        ,binary_to_pb/3
        ,list_to_number/1
        ,to_float/1
        ,random_lists/2
        ,merge_num_elems/2
        ,merge_num_elems/3
        ,list_nth/2
        ,get_socket_info/1
        ,list_add/3
        ,datetime_to_timestamp/1
    ]
).
-include("common.hrl").
-include("protocol.hrl").

%% @doc 检查进程是否存活(可检查远程节点上的进程)
%% <div>注意: 此函数的消耗比较高，非必要时不要使用</div>
-spec is_process_alive(pid()) -> true | false.
is_process_alive(P) when is_pid(P) ->
    case rpc:call(node(P), erlang, is_process_alive, [P]) of
        true -> true;
        false -> false;
        _ -> false
    end.

%% @doc 程序暂停执行时长(单位:毫秒)
-spec sleep(T::integer()) -> ok.
sleep(T) ->
    receive
    after
        T -> ok
    end.

%% @doc 将true,false原子转成对应的0,1整数
-spec bool2int(X::boolean()) -> 0 | 1.
bool2int(true) -> 1;
bool2int(false) -> 0.

%% @doc 模拟for循环
-spec for(Begin::integer(), End::integer(), Fun::function()) -> ok.
for(End, End, Fun) ->
    Fun(End),
    ok;
for(Begin, End, Fun) when Begin < End ->
    Fun(Begin),
    for(Begin + 1, End, Fun).

%% @doc 生成16位格式的md5值
-spec md5(iodata()) -> binary().
md5(Data) ->
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(Data))]).

%% @doc 用 Key 作为秘钥生成 Data 的 16 位格式的 hmac-sha1 值
-spec hmac_sha1(Key :: iodata(), Data :: iodata()) -> binary().
hmac_sha1(Key, Data) ->
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(crypto:hmac(sha, Key, Data))]).

%% @doc 取小于X的最大整数 
-spec floor(number()) -> integer().
floor(X) ->
    T = erlang:trunc(X),
    case X < T of
        true -> T - 1;
        _ -> T
    end.

%% @doc 取大于X的最小整数
-spec ceil(number()) -> integer().
ceil(X) ->
    T = erlang:trunc(X),
    case X > T of
        true -> T + 1;
        _ -> T
    end.

%% @doc 限制最大最小值
-spec check_range(Val, Min, Max) -> number() when
    Val :: number(),
    Min :: number(),
    Max :: number().
check_range(Val, Min, Max) ->
    if
        Val > Max -> Max;
        Val < Min -> Min;
        true -> Val
    end.

%% @doc 限制最大值
-spec check_max(Val::number(), Max::number()) -> number().
check_max(Val, Max) ->
    if
        Val > Max -> Max;
        true -> Val
    end.

%% @doc 限制最小值
-spec check_min(Val::number(), Min::number()) -> number().
check_min(Val, Min) ->
    if
        Val < Min -> Min;
        true -> Val
    end.

%% @doc 以行模式读取文件
-spec readlines(string()) -> {ok, list()} | {error, term()}.
readlines(FileName) ->
    case file:open(FileName, [read]) of
        {error, Reason} -> {error, Reason};
        {ok, F} -> get_all_lines(F, [])
    end.
get_all_lines(F, L) ->
    case io:get_line(F, "") of
        {error, Reason} -> {error, Reason};
        eof -> {ok, lists:reverse(L)};
        Line -> get_all_lines(F, [Line | L])
    end.

%% 日期转时间戳 DateTime :: {{2013,11,13}, {18,0,0}}
-spec datetime_to_timestamp(DateTime :: {{non_neg_integer(), non_neg_integer(), non_neg_integer()},{non_neg_integer(), non_neg_integer(), non_neg_integer()}}) -> non_neg_integer().
datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) -
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}}).

%% @doc 读取文件，并将内容转成term()
-spec load(string()) ->
    {ok, undefined} | {ok, term()} | {error, term()}.
load(File) ->
    case file:consult(File) of
        {error, Reason} -> {error, Reason};
        {ok, []} -> {ok, []};
        {ok, [Term]} -> {ok, Term}
    end.

%% @doc 将一个term()写入文件
-spec save(string(), term()) -> ok | {error, term()}.
save(File, Term) ->
    case file:open(File, [write]) of
        {error, Reason} -> {error, Reason};
        {ok, F} ->
            io:format(F, "~p.", [Term]),
            file:close(F),
            ok
    end.

%% @doc 替换模板变量
-spec template(string(), [{atom(), string()}]) -> string() | {error, term()}.
template(File, Vars) ->
    case file:read_file(File) of
        {error, Reason} -> {error, Reason};
        {ok, Content} ->
            V = [{lists:concat(["{{", K, "}}"]), to_list(V)} || {K, V} <- Vars],
            %% template_replace(bitstring_to_list(Content), V)
            template_replace(unicode:characters_to_list(Content), V)
    end.
template_replace(Text, []) -> Text;
template_replace(Text, [{K, V} | L]) ->
    %% 用re:replace性能差到不能看...
    %% T = re:replace(Text, K, V, [caseless, global]),
    T = replace(Text, K, V),
    template_replace(T, L).

%% @doc 替换字符串
%% @todo 有必要再优化下性能
-spec replace(string(), string(), string()) -> string().
replace([], _Search, _Replace) -> "";
replace(Str, Search, Replace) ->
    replace(Str, Search, Replace, length(Search), []).
replace(Str, Search, Replace, Len, Rtn) ->
    case string:str(Str, Search) of
        0 -> Rtn ++ Str;
        P ->
            S = string:substr(Str, 1, P - 1) ++ Replace,
            replace(string:substr(Str, P + Len), Search, Replace, Len, Rtn ++ S)
    end.

%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand(Min::integer(), Max::integer()) -> integer().
rand(Min, Max) when Max < Min ->
    ?ERR("随机数错误的区间:[~w:~w]",[Min, Max]),
    rand(Max, Min);
rand(Min, Min) -> Min;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    %% @todo 这个机制有必要改进下
    case get(rand_seed) of
        undefined ->
            Seed = case catch sys_rand:get_seed() of
                N = {_, _, _} -> N;
                %% _ -> erlang:now()
                _ -> os:timestamp()
            end,
            random:seed(Seed),
            put(rand_seed, Seed);
        _ ->
            ignore
    end,
    M = Min - 1,
    random:uniform(Max - M) + M.

%% @doc 从一个list中随机取出一项
-spec rand_list(List::list()) -> undefined | term().
rand_list([]) -> undefined;
rand_list([I]) -> I;
rand_list(List) -> 
    Idx = rand(1, length(List)),
    get_term_from_list(List, Idx).
get_term_from_list([H | _T], 1) ->
    H;
get_term_from_list([_H | T], Idx) ->
    get_term_from_list(T, Idx - 1).

%% @doc 从一个列表中取N个不同的值，组成一个列表
%% 运行时间:List=1~100000 30+ ms
random_lists(_List, Num) when Num =< 0 -> [];
random_lists([], _Num) -> [];
random_lists(List = [H|_], Num) ->
    ListSize = length(List),
    Num1 = case ListSize >= Num of
        true -> Num;
        false -> ListSize
    end,
    Result = case is_number(H) of
        true ->
            do_random_lists(List, ListSize, Num1, []);
        false ->
            List1 = lists:seq(1, ListSize),
            List2 = do_random_lists(List1, ListSize, Num1, []),
            [lists:nth(Idx, List) || Idx <- List2]
    end,
    Result.
do_random_lists(_, _, 0, Result) -> lists:reverse(Result);
do_random_lists(List = [H|T], ListSize, N, Result) ->
    Idx = rand(1, ListSize),
    {Elem, List1} = case Idx of
        1 -> {H, T};
        ListSize ->
            [A|B] = lists:reverse(List),
            {A, lists:reverse(B)};
        _ ->
            {A, [B|C]} = lists:split(Idx-1, List),
            {B, A ++ C}
    end,
    do_random_lists(List1, ListSize-1, N-1, [Elem|Result]).

%% @doc 从一个list中按各项权重值随机取出一项 每项为tuple()
-spec rand_list(List::[tuple()], Pos::pos_integer()) -> undefined | tuple().
rand_list([], _Pos) -> undefined;
rand_list([I], _Pos) -> I;
rand_list(List, Pos) ->
    Sum = lists:sum([element(Pos, I) || I <- List]),
    RandVal = rand(1, Sum),
    get_rand_tuple(List, Pos, RandVal).
get_rand_tuple([H | T], Pos, RandVal) ->
    Rand = element(Pos, H),
    case RandVal =< Rand of
        true -> H;
        false -> get_rand_tuple(T, Pos, RandVal - Rand)
    end.

%% @doc 将tuplelist转成一个record字串(tuplelist中必须要有record_name这一项)
%% <div>注意: 此函数比较低效，在要求性能的情况下不能使用</div>
-spec tuplelist_to_record_string([{atom(), any()}]) -> string() | {error, tuplelist_to_record_not_found_record_name}.
tuplelist_to_record_string(L) ->
    case lists:keyfind(record_name, 1, L) of
        false -> {error, tuplelist_to_record_not_found_record_name};
        {_, RecName} ->
            Nl = lists:keydelete(record_name, 1, L),
            io_lib:format("#~s{~s}", [RecName, rec_items(Nl, [])])
    end.
rec_items([], L) -> string:join(lists:reverse(L), ", ");
rec_items([{K, V} | T], L) ->
    S = io_lib:format("~s = ~p", [K, V]),
    rec_items(T, [S | L]).

%% @doc 返回格式化的二进制字符串
%% <ul>
%% <li>String: 待格式化的二进制字符串</li>
%% <li>Args: 格式化参数，跟{@link io_lib:format/3}相同</li>
%% </ul>
-spec fbin(bitstring() | string(), list()) -> binary().
fbin(String, Args) when is_bitstring(String) ->
    list_to_binary(io_lib:format(String, Args));
fbin(String, Args) ->
    fbin(list_to_bitstring(String), Args).

%% @doc 把二进制字符串合并
-spec bjoin(BitstringList, Sep) -> bitstring() when
    BitstringList :: [bitstring()],
    Sep :: bitstring().
bjoin(BitstringList, Sep) ->
    bjoin(BitstringList, Sep, <<"">>).
bjoin([], _, Result) -> Result;
bjoin([String], _, <<"">>) -> String;
bjoin([String], Sep, Result) ->
    fbin(<<"~s~s~s">>, [Result, Sep, String]);
bjoin([String|T], Sep, Result) ->
    Result1 = case Result of
        <<"">> -> String;
        _ -> fbin(<<"~s~s~s">>, [Result, Sep, String])
    end,
    bjoin(T, Sep, Result1).

%% @doc 在控制台显示带中文的字符串
-spec cn(String) -> ok when
    String :: bitstring() | string().
cn(String) ->
    cn(String, []).

%% @doc 在控制台显示带中文的字符串
%% <ul>
%% <li>F: 待显示的中文字符串（可带格式化参数）</li>
%% <li>A: 格式化参数</li>
%% </ul>
-spec cn(F, A) -> ok when
    F :: string() | bitstring(),
    A :: [term()].
cn(F, A) ->
    io:format("~ts", [iolist_to_binary(io_lib:format(F, A))]).

%% @doc 将任意类型的数据转成string()类型
-spec to_string(any()) -> string().
to_string(X) -> lists:flatten(io_lib:format("~w", [X])).

%% @doc 把所有

%% @doc 将任意类型的数据转成list()类型(主要用于控制台打印).
%% <div>注意:tuple类型有特殊处理</div>
-spec to_list(any()) -> list().
to_list(X) when is_integer(X)     -> integer_to_list(X);
to_list(X) when is_float(X)       -> float_to_list(X);
to_list(X) when is_atom(X)        -> atom_to_list(X);
to_list(X) when is_binary(X)      -> binary_to_list(X);
to_list(X) when is_pid(X)         -> pid_to_list(X);
to_list(X) when is_function(X)    -> erlang:fun_to_list(X);
to_list(X) when is_port(X)        -> erlang:port_to_list(X);
to_list(X) when is_tuple(X)       -> to_string(X);
to_list(X) when is_list(X)        -> X.

%% @doc 将list()转成数字
-spec list_to_number(list()) -> number().
list_to_number(X) when is_number(X) -> X;
list_to_number([]) -> 0;
list_to_number(X) when is_bitstring(X) -> list_to_number(to_list(X));
list_to_number(X) when is_list(X) ->
    case string:to_integer(X) of
        {error, no_integer} ->
            case string:to_float(X) of
                {error, no_float} -> undefined;
                {Float, _} -> Float
            end;
        {Int, _} -> Int
    end.

%% @doc 将 list() 转成 float
-spec to_float(list()) -> number().
to_float(X) when is_number(X) -> X;
to_float([]) -> 0;
to_float(X) when is_bitstring(X) -> to_float(to_list(X));
to_float(X) when is_list(X) ->
        case string:to_float(X) of
            {error, no_float} ->
                case string:to_integer(X) of
                    {error, no_integer} -> undefined;
                    {Int, _} -> Int
                end;
            {Float, _} -> Float
        end.

%% @doc 将任意类型的数据转成atom()类型
-spec to_atom(any()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) ->
    L = to_list(X),
    list_to_atom(L).

%% @doc 根据字符串内容生成函数
-spec build_fun(string()) -> function().
build_fun(String)->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, L} = erl_parse:parse_exprs(Tokens),
    B = erl_eval:new_bindings(),
    BS = erl_eval:bindings(B),
    {[F], []} = erl_eval:expr_list(L, BS),
    F.

%% @doc 解析 QueryString
-spec parse_qs(string()) -> list().
parse_qs(String) when is_bitstring(String) -> parse_qs(bitstring_to_list(String));
parse_qs(String) -> parse_qs(String, "&", "=").

%% @doc 按指定的字符切割字符串
-spec parse_qs(String, Token1, Token2) -> list() when
    String :: bitstring() | string(),
    Token1 :: list(),
    Token2 :: list().
parse_qs(String, Token1, Token2) when is_bitstring(String) ->
    parse_qs(bitstring_to_list(String), Token1, Token2);
parse_qs(String, Token1, Token2) ->
    [list_to_tuple(string:tokens(Kv, Token2)) || Kv <- string:tokens(String, Token1)].

%% @doc term序列化，term转换为string格式
-spec term_to_string(term()) -> string().
term_to_string(Term) -> io_lib:format("~w", [Term]).

%% @doc term序列化，term转换为bitstring
-spec term_to_bitstring(term()) -> bitstring().
term_to_bitstring(Term) -> list_to_bitstring(term_to_string(Term)).

%% @doc term反序列化，string转换为term
-spec string_to_term(String) -> {error, Reason} | {ok, term()} when
    String :: undefined | string() | bitstring(),
    Reason :: term().
string_to_term(undefined) -> {ok, undefined};
string_to_term("undefined") -> {ok, undefined};
string_to_term(String) when is_bitstring(String) ->
    string_to_term(binary_to_list(String));
string_to_term(String) ->
    S = re:replace(String, "<[0-9]+\\.[0-9]+\\.[0-9]+>", "undefined", [{return, list}, global]),
    case erl_scan:string(S ++ ".") of
        {ok, Tokens, _} -> erl_parse:parse_term(Tokens);
        {error, Err, _} -> {error, Err}
    end.

%% @doc 将列里的不同类型转行成字节型
%% @todo 貌似不够高效，可以优化下
%% <div>如 [&lt;&lt;"字节"&gt;&gt;, 123, asd, "assd"] 输出 &lt;&lt;"字节123asdassd"&gt;&gt;</div>
-spec all_to_binary(list()) -> binary().
all_to_binary(List) -> all_to_binary(List, []).
all_to_binary([], Result) -> list_to_binary(Result);
all_to_binary([P | T], Result) when is_list(P) ->
    all_to_binary(T, lists:append(Result, P));
all_to_binary([P | T], Result) when is_integer(P) ->
    all_to_binary(T, lists:append(Result, integer_to_list(P)));
all_to_binary([P | T], Result) when is_binary(P) ->
    all_to_binary(T, lists:append(Result, binary_to_list(P)));
all_to_binary([P | T], Result) when is_float(P) ->
    all_to_binary(T, lists:append(Result, float_to_list(P)));
all_to_binary([P | T], Result) when is_atom(P) ->
    all_to_binary(T, lists:append(Result, atom_to_list(P))).

%% @doc 将Val值转换为binary格式（8位二进制）
%% @todo 貌似没有什么用，考虑删除掉
-spec to_binary(integer()) -> binary().
to_binary(Val) when is_integer(Val) -> list_to_binary(integer_to_list(Val));
to_binary(Val) when is_float(Val) -> list_to_binary(float_to_list(Val));
to_binary(Val) when is_list(Val) -> list_to_binary(Val);
to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_atom(Val) -> to_binary(atom_to_list(Val));
to_binary(_Val) -> <<>>.

%% @doc 检查名称（会对长度、特殊字符屏蔽、敏感词屏蔽）
-spec check_name(bitstring(), WidthMin :: pos_integer(), WidthMax :: pos_integer()) -> ok | {false, Reason :: atom()}.
check_name(Text, WidthMin, WidthMax) ->
    case name_len_valid(Text, WidthMin, WidthMax) of
        false -> {false, size};%% 名称长度不合要求
        true ->
            %% 检查字符有效性
            case name_valid(Text) of
                false -> {false, characters}; %% 名称中含有系统限制字符
                true ->
                    %% 检查禁用词
                    case lists:member(Text, keywords:banned()) of
                        true -> {false, keyword};%% 请勿使用非法词汇
                        false -> ok
                    end
            end
    end.

%% @doc 检查文本是否含有英文半角符号等半角符号
-spec is_normal_text(Text::iodata()) -> boolean().
is_normal_text(Text) ->
    %% 反斜杠"\"的匹配，使用了双重转义，因为erlang里面"\"字符本身也需要转义
    case re:run(Text, "[\;\^\,\.\"\'\:\+\=\!\?\<\>\/\*\|\~\`\@\#\$\%\(\)\{\}\\\-\\\[\\\]\\\\]", [{capture, none}, caseless, unicode]) of
        match -> false;
        nomatch -> true
    end.

%% @doc 检查文本中是否含有禁用词
-spec text_banned(Text::iodata()) -> boolean().
text_banned(Text) ->
    case is_normal_text(Text) of
        false -> true;
        true ->
            case text_banned(Text, keywords:banned()) of
                true -> true;
                false -> text_banned(Text, keywords:banned())
            end
    end.

%% @doc 检查文本中各项
-spec check_text(Text::iodata(), Len::non_neg_integer()) -> {false, bitstring()} | ok. 
check_text(Text, Len) ->
    case name_len_valid(Text, 0, Len) of
        false -> {false, ?DEBUG("长度不合要求")};
        true ->
            %% 检查字符有效性
            case name_valid(Text) of
                false -> {false, ?DEBUG("不能含有系统限制字符")};
                true ->
                    %% 检查禁用词
                    case text_banned(Text) of
                        true -> {false, ?DEBUG("请勿使用非法词汇")};
                        false -> ok
                    end
            end
    end.

%% @doc 检查文本中各项
-spec check_text(Text::iodata(), Len::non_neg_integer(), Key::iodata()) -> {false, atom()} | ok. 
check_text(Text, Len, _Key) ->
    case name_len_valid(Text, 0, Len) of
        false -> {false, err_illegal_len};
        true ->
            %% 检查字符有效性
            case name_valid(Text) of
                false -> {false, err_illegal_text};
                true ->
                    %% 检查禁用词
                    case text_banned(Text) of
                        true -> {false, err_illegal_text};
                        false -> ok
                    end
            end
    end.

%% @doc 文字内容过滤，将关键词替换为"*"
%% <div>注意:使用的是默认关键启库,keywords.erl</div>
-spec text_filter(Text::iodata()) -> NewText::bitstring().
text_filter(Text) ->
    T1 = text_filter(Text, kwywords:banned()),
    T2 = text_filter(T1, kwywords:fuck_hexie()),
    list_to_bitstring(T2).

%% @doc 文字内容过滤，将关键词替换为"*"
-spec text_filter(Text::iodata(), Keywords::[string()]) -> string().
text_filter(Text, []) -> Text;
text_filter(Text, [H | L]) ->
    T = re:replace(Text, H, "\*", [caseless, global]),
    text_filter(T, L).

%% @doc 计算剩余时间，单位：毫秒
-spec time_left(TimeMax::integer(), Begin::erlang:timestamp()) -> integer().
time_left(TimeMax, Begin)->
    T = util:floor(TimeMax - timer:now_diff(erlang:now(), Begin) / 1000),
    case T > 0 of
        true -> T;
        false -> 0
    end.

%% 字符串长度计算(支持中文)
-spec utf8_len(Str::bitstring()) -> error | non_neg_integer().
utf8_len(Str) ->
    case unicode:characters_to_list(Str) of
        {error, _Reason} -> error;
        {ok, CharList} ->
            string_width(CharList)
    end.

%% 对大列表进行'--'操作
-spec sub_big_list(L1::list(), L2::list()) -> list().
sub_big_list(L1, L2) ->
    H1 = ordsets:from_list(L1),
    H2 = ordsets:from_list(L2),
    ordsets:subtract(H1, H2).

%% 把同样的对象克隆若干份，放进列表中
-spec one_to_list(Obj::term(), Num::non_neg_integer()) -> list().
one_to_list(Obj, Num) ->
    one_to_list(Obj, Num, []).
one_to_list(_, 0, Result) -> Result;
one_to_list(Obj, N, Result) -> one_to_list(Obj, N-1, [Obj|Result]).

%% @doc 获取调用栈信息
get_stacktrace() ->
    try 
        throw(a)
    catch
        _Type:_Err ->
            tl(erlang:get_stacktrace())
    end.

%% @doc 关闭dets，有错则打印
close_dets(Name) ->
    case catch dets:close(Name) of
        ok -> ok;
        _Err -> ?ERR("关闭dets[~w]失败:~w", [Name, _Err])
    end.

%% @doc 列表去重
-spec remove_duplicate(L :: list()) -> list().
remove_duplicate([]) -> [];
remove_duplicate(L = [_|_]) -> sets:to_list(sets:from_list(L)).

%% @doc Basho的protobuf库encode出来的结果 =》 二进制
pb_to_binary(PbEncodedData) -> {ok, iolist_to_binary(PbEncodedData)}.

%% @doc 二进制 =》 {ok, protobuf 解析出来的record} | {error, undefined}
binary_to_pb(Parser, DecodeRec, Bin) ->
    case catch Parser:decode(DecodeRec, Bin) of
        Rec when is_tuple(Rec) ->
            case is_record(Rec, DecodeRec) of
                true -> {ok, Rec};
                false -> {error, undefined}
            end;
        _ -> {error, undefined}
    end.

%% @doc 合并tuple()的数字元素
merge_num_elems(T1, T2) ->
    merge_num_elems(T1, T2, fun(A, B) -> A + B end).
merge_num_elems(T1, T2, Fun) ->
    Size = tuple_size(T1),
    merge_num_elems(T1, T2, 0, Size, Fun).
merge_num_elems(T1, _T2, N, N, _Fun) -> T1;
merge_num_elems(T1, T2, M, N, Fun) when M < N ->
    Idx = M+1,
    E1 = erlang:element(Idx, T1),
    E2 = erlang:element(Idx, T2),
    case is_number(E1) andalso is_number(E2) of
        true ->
            merge_num_elems(erlang:setelement(Idx, T1, Fun(E1, E2)), T2, Idx, N, Fun);
        false ->
            merge_num_elems(T1, T2, Idx, N, Fun)
    end.

%% @doc 查找该元素在列表中的下标
list_nth(E, L) when is_list(L) ->
    list_nth(E, L, 1).
list_nth(_, [], _) -> 0;
list_nth(E, [E|_], Idx) -> Idx;
list_nth(E, [_|T], Idx) ->
    list_nth(E, T, Idx+1).

%% @doc 获取socket中的ip和port信息
-spec get_socket_info(Socket :: port()) -> {Ip :: term(), Port :: unknown | non_neg_integer()}.
get_socket_info(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, Port}} -> {Ip, Port};
        _ -> {unknown, unknown}
    end.

%% @doc 列表累加
-spec list_add(Fun :: function(), [term()], term()) -> term().
list_add(_Fun, [], Result) -> Result;
list_add(Fun, [H|T], Result) ->
    Result1 = Fun(H, Result),
    list_add(Fun, T, Result1).


%% -----------------------------------------
%% 私有函数
%% -----------------------------------------

text_banned(_Text, []) -> false;
text_banned(Text, [H | T]) ->
    case re:run(Text, H, [{capture, none}, caseless]) of
        match -> true;
        _ -> text_banned(Text, T)
    end.

%% 检查名称长度规范
name_len_valid(Text, Min, Max) ->
    case unicode:characters_to_list(Text) of
        {error, _Reason} -> false;
        {ok, CharList} ->
            Len = string_width(CharList),
            ?DEBUG("名字的长度为==========================================~w, Min=~w,Max=~w",[Len,Min,Max]),
            Len =< Max andalso Len >= Min
    end.

%% 检查名字：只允许使用汉字(不含标点符号)、字母、数字和下划线
name_valid(Text) ->
    %% 貌似测试只有bitstring才能正确执行到结果
    %% unicode字符集：
    %% {FF00}-{FFEF} 通用ASCII全角标点符号集
    %% {3000}-{303F} 中日韩标点符号集
    %% {4E00}-{9FBF} 中日韩统一汉字
    case re:run(Text, "[^a-zA-Z0-9\\x{4E00}-\\x{9FA5}_\\x{306E}\\x{2116}\\x{706c}\\x{4e36}\\x{4e28}\\x{4e3f}\\x{2573}\\x{256c}\\x{5350}\\x{2103}\\x{0021}]", [{capture, none}, caseless, unicode]) of
        match ->
            false; %%<<"角色名只允许使用汉字、字母、数字和下划线">>}; %% 含有非法字符 
        nomatch ->
            case lists:member(Text, special_word()) of
                true ->
                    false;
                false->
                    true
            end
    end.

%% 一些处理不了的特殊字符 33是英文!
special_word() ->
    [[33]]. 

%% 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
string_width(String) ->
    string_width(String, 0).
string_width([], Len) ->
    Len;
string_width([H | T], Len) ->
    case H > 255 of
        true ->
            string_width(T, Len + 2);
        false ->
            string_width(T, Len + 1)
    end.
