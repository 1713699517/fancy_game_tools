%%----------------------------------------------------
%% 客户端连接验证
%%----------------------------------------------------
-module(proto_10).
-export([
        info/0
        ,cfg/0
    ]
).
-include("protocol.hrl").

info() -> {
        "客户端连接区服验证模块"
        ,[]
    }.

cfg() ->
    [
        #rpc{
            code = 1001
            ,req_desc = "登录区服务器"
            ,req = [
                {string, account, "玩家帐号"}
                ,{string, platform, "所选平台"}
                ,{uint16, zone_id, "所选分区"}
                ,{string, session_id, "SessionId（SDK服务器返回给客户端的）"}
            ]
            ,reply = [
                {uint8, result, "验证结果(0-失败 1-成功)"}
                ,{string, msg, "附加消息"}
            ]
        }

    ].
