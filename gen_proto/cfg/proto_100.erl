%%----------------------------------------------------
%% 角色信息获取/初始化
%%----------------------------------------------------
-module(proto_100).
-export([
        info/0
        ,cfg/0
    ]
).
-include("protocol.hrl").
-include("proto_def.hrl").

info() -> {
        "角色信息获取/初始化"
        ,["common.hrl", "role.hrl"]
    }.

cfg() ->
    [
        #rpc{
            code = 10000
            ,req_desc = "初始化角色信息"
            ,req = [
            ]
            ,reply = [
                {uint32, rid, "角色ID"}
                ,{string, platform, "所属平台名"}
                ,{uint16, zone_id, "所属分区"}
                ,{string, name, "角色名"}
                ,{uint8, lev, "等级"}
                ,{uint32, hp_max, "气血上限"}
                ,{uint32, hp, "气血"}
            ]
        }
        
    ].
