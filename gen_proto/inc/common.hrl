%%----------------------------------------------------
%% 公共定义文件
%% (不要随意在此添加新的定义)
%%----------------------------------------------------
-define(DB, mysql_conn_pool).
-define(CLIENT_WEB, 1).
-define(CLIENT_IOS, 2).
-define(CLIENT_ANDROID, 3).

%% 数字型的bool值
-define(false, 0).
-define(true, 1).

%% 通用比例的分母
-define(COM_DEN, 10000).

%% 定义数字的上限值
-define(MAX_UINT, 3000000000).

%% 字体颜色类型
-define(color_white, 0).  %% 白色
-define(color_green, 1).  %% 绿色
-define(color_blue, 2).   %% 蓝色
-define(color_purple, 3). %% 紫色
-define(color_orange, 4). %% 橙色
-define(color_yellow, 5). %% 黃色
-define(color_red, 6).    %% 红色
-define(color_gray, 7).   %% 灰色

%% 写入队列数量
-define(db_queue_num, 5).

%% 语言翻译，返回给玩家的文本信息需要经过此宏的转换
-define(T(Text), lang:get(Text)).

%% 返回格式化字符串，等价于io_lib:format/2
-define(S(F, A), io_lib:format(F, A)).

%% 自定格式信息输出，相当于io:format，支持中文输出
-define(P(F, A),
    case os:type() of
        {win32, _} -> io:format(F, A);
        _ -> io:format("~ts", [io_lib:format(F, A)])
    end).
-define(P(F), ?P(F, [])).
%% 按固定格式输出调试信息，非debug模式下自动关闭
-ifdef(debug).
-define(DEBUG(Msg), logger:debug(Msg, [], ?MODULE, ?LINE)).
-define(DEBUG(F, A), logger:debug(F, A, ?MODULE, ?LINE)).
-else.
-define(DEBUG(Msg), ok).
-define(DEBUG(F, A), ok).
-endif.
%% 按固定格式输出普通信息到控制台
-define(INFO(Msg), logger:info(Msg, [], ?MODULE, ?LINE)).
-define(INFO(F, A), logger:info(F, A, ?MODULE, ?LINE)).
%% 按固定格式输出错误信息到控制台
-define(ERR(Msg), logger:error(Msg, [], ?MODULE, ?LINE)).
-define(ERR(F, A), logger:error(F, A, ?MODULE, ?LINE)).

%% 是否验证登录
-ifdef(debug).
-define(is_validate_login, false).
-else.
-define(is_validate_login, true).
-endif.

%% 自定义类型:角色ID
-type role_id() :: {pos_integer(), bitstring(), non_neg_integer()}.
%% 自定义类型:单位ID
-type unit_id() :: {pos_integer(), pos_integer()}.

%% 中央服信息数据结构
-record(center, {
        %% 标识中央服是否连接
        ready = 0           :: ?true | ?false
        %% 中央服镜像进程pid(远端)
        ,pid                :: undefined | pid()
        %% 中央服节点名
        ,node               :: atom()
        %% 本地平台标识
        ,platform = <<>>    :: bitstring()
        %% 本地游戏区号
        ,zone_id = 0        :: non_neg_integer()
        %% 合服信息
        ,combine = []       :: [{bitstring(), non_neg_integer()}]
    }
).

%% 带catch的gen_server:call/2，返回{error, timeout} | {error, noproc} | {error, term()} | term() | {exit, normal}
%% 此宏只会返回简略信息，如果需要获得更详细的信息，请使用以下方式自行处理:
%% case catch gen_server:call(Pid, Request)
-define(CALL(_Call_Pid, _Call_Request),
    case catch gen_server:call(_Call_Pid, _Call_Request) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', {noproc, _}} -> {error, noproc};
        {'EXIT', {normal, _}} -> {error, exit};
        {'EXIT', _Call_Err} -> {error, _Call_Err};
        _Call_Return -> _Call_Return
    end
).

-define(CALL(_Call_Pid, _Call_Request, _CALL_TIMEOUT),
    case catch gen_server:call(_Call_Pid, _Call_Request, _CALL_TIMEOUT) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', {noproc, _}} -> {error, noproc};
        {'EXIT', {normal, _}} -> {error, exit};
        {'EXIT', _Call_Err} -> {error, _Call_Err};
        _Call_Return -> _Call_Return
    end
).

%% 未实现的函数请加入下面这个宏到函数体中
-define(NYI, io:format("*** NYI ~p ~p~n", [?MODULE, ?LINE]), exit(nyi)).

%% 将record转换成tuplelist
-define(record_to_tuplelist(Rec, Ref), lists:zip([record_name | record_info(fields, Rec)], tuple_to_list(Ref))).

