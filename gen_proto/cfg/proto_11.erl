%%----------------------------------------------------
%% 客户端创建角色、登陆流程相关
%%----------------------------------------------------
-module(proto_11).
-export([
        info/0
        ,cfg/0
    ]
).
-include("protocol.hrl").

info() -> {
        "客户端连接验证模块"
        ,["common.hrl", "role.hrl"]
    }.

cfg() ->
    [
        #rpc{
            code = 1101
            ,req_desc = "查询角色列表"
            ,req = [
            ]
            ,reply = [
                {uint8, result, "结果(0-失败 1-成功)"}
                ,{string, msg, "附加消息"}
                ,{array, tuple, role_list, "角色列表", [
                        {tuple, id, [
                                {uint32, rid, "角色ID"}
                                ,{string, platform, "所属平台名"}
                                ,{uint16, zone_id, "所属分区"}
                            ]
                        }
                        ,{string, name, "角色名"}
                    ]
                }
            ]
        }

        ,#rpc{
            code = 1102
            ,req_desc = "创建角色"
            ,req = [
                {string, name, "角色名字"}
            ]
            ,reply = [
                {uint8, result, "结果(0-失败 1-成功)"}
                ,{string, msg, "附加消息"}
                ,{tuple, id, [
                        {uint32, rid, "角色ID"}
                        ,{string, platform, "所属平台名"}
                        ,{uint16, zone_id, "所属分区"}
                    ]
                }
            ]
        }

        ,#rpc{
            code = 1103
            ,req_desc = "登陆角色"
            ,req = [
                {uint32, rid, "角色ID"}
                ,{string, platform, "所属平台名"}
                ,{uint16, zone_id, "所属分区"}
            ]
            ,reply = [
                {uint8, result, "结果(0-失败1-成功)"}
                ,{string, msg, "附加消息"}
            ]
        }

        ,#rpc{
            code = 1199
            ,req_desc = "心跳"
            ,req = [
            ]
            ,reply = [
                {uint32, ts, "时间戳"}
            ]
        }

    ].
