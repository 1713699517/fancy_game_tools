%%----------------------------------------------------
%% 地图广播
%%----------------------------------------------------
-module(proto_101).
-export([
        info/0
        ,cfg/0
    ]
).
-include("protocol.hrl").
-include("proto_def.hrl").

info() -> {
        "地图信息推送、广播"
        ,["common.hrl", "map.hrl", "role.hrl"]
    }.

cfg() ->
    [
        %% ================================================
        %% 操作类消息
        %% ================================================
        #rpc{
            code = 10100
            ,req_desc = "请求进入地图"
            ,req = [
            ]
            ,reply = [
                {uint8, result, "结果"}
                ,{string, msg, "附加消息"}
            ]
        }

        ,#rpc{
            code = 10101
            ,req_desc = "地图资源加载已完成，可以推送单位、角色列表等等信息"
            ,req = [
            ]
            ,reply = [
            ]
        }

        ,#rpc{
            code = 10102
            ,req_desc = "请求移动"
            ,req = [
                {uint32, map_id, "地图ID"}
                ,{uint16, x, "目标坐标x"}
                ,{uint16, y, "目标坐标y"}
                ,{uint8, dir, "目标朝向（1：上 2：右上~8：左上）"}
            ]
            ,reply = [
                {uint8, result, "结果"}
                ,{string, msg, "附加消息"}
            ]
        }

        ,#rpc{
            code = 10103
            ,req_desc = "移动过程中同步位置"
            ,req = [
                {uint32, map_id, "地图ID"}
                ,{uint16, x, "目标坐标x"}
                ,{uint16, y, "目标坐标y"}
                ,{uint8, dir, "目标朝向（1：上 2：右上~8：左上）"}
            ]
            ,reply = [
                {uint8, result, "结果"}
                ,{string, msg, "附加消息"}
            ]
        }

        

        %% ================================================
        %% 通知类消息
        %% ================================================
        ,#rpc{
            code = 10120
            ,req = [
            ]
            ,reply_desc = "通知客户端角色已进入地图，加载地图资源"
            ,reply = [
                {uint32, map_id, "地图ID"}
                ,{uint32, base_id, "地图基础ID"}
                ,{uint16, x, "目标坐标x"}
                ,{uint16, y, "目标坐标y"}
            ]
        }
        ,#rpc{
            code = 10121
            ,req = [
            ]
            ,reply_desc = "通知可视范围内的角色列表变化"
            ,reply = [
                {array, map_role, role_list, "角色列表", [
                        ?MAP_ROLE
                    ]
                }
            ]
        }

        %% ================================================
        %% 广播类消息
        %% ================================================
        ,#rpc{
            code = 10150
            ,req = [
            ]
            ,reply_desc = "广播进入可视范围/离开可视范围的角色列表"
            ,reply = [
                {array, map_role, enter_role_list, "进入可视范围的角色列表", [
                        ?MAP_ROLE
                        ,{uint16, last_move_src_x, "最近一次移动起始坐标x"}
                        ,{uint16, last_move_src_y, "最近一次移动起始坐标y"}
                        ,{uint16, last_move_dest_x, "最近一次移动目的坐标x"}
                        ,{uint16, last_move_dest_y, "最近一次移动目的坐标y"}
                        ,{uint8, last_move_dir, "最近一次移动朝向"}
                    ]
                }
                ,{array, tuple, leave_role_list, "离开可视范围的角色列表", [
                        {uint32, rid, "角色ID"}
                        ,{string, platform, "角色平台"}
                        ,{uint16, zone_id, "角色分区"}
                    ]
                }
            ]
        }
        ,#rpc{
            code = 10151
            ,req = [
            ]
            ,reply_desc = "广播角色移动"
            ,reply = [
                {array, tuple, role_list, "角色列表", [
                        {uint32, rid, "角色ID"}
                        ,{string, platform, "角色平台"}
                        ,{uint16, zone_id, "角色分区"}
                        ,{uint16, x, "坐标x"}
                        ,{uint16, y, "坐标y"}
                        ,{uint16, dest_x, "移动目标坐标x"}
                        ,{uint16, dest_y, "移动目标坐标y"}
                        ,{uint8, dir, "移动朝向"}
                    ]
                }
            ]
        }
        ,#rpc{
            code = 10152
            ,req = [
            ]
            ,reply_desc = "广播角色离开地图"
            ,reply = [
                {array, tuple, role_list, "角色列表", [
                        {uint32, rid, "角色ID"}
                        ,{string, platform, "角色平台"}
                        ,{uint16, zone_id, "角色分区"}
                        ,{uint32, map_id, "离开的地图ID"}
                        ,{uint32, base_id, "离开的地图基础ID"}
                    ]
                }
            ]
        }
        ,#rpc{
            code = 10153
            ,req = [
            ]
            ,reply_desc = "广播地图区域内角色信息变化"
            ,reply = [
                {array, map_role, role_list, "角色列表", [
                        ?MAP_ROLE
                    ]
                }
            ]
        }
    ].
