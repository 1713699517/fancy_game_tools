%%----------------------------------------------------
%% 协议常用默认定义配置结构
%%----------------------------------------------------

-define(MAP_ROLE, 
    {uint32, rid, "所属角色ID"}
    ,{string, platform, "所属角色平台"}
    ,{uint16, zone_id, "所属角色分区"}
    ,{string, name, "名称"}
    ,{uint8, lev, "单位等级"}
    ,{uint8, status, "单位状态(0:正常 1:死亡 2:战斗中 ...)"}
    ,{uint8, action, "动作状态(0:无动作 1:战斗中 2:采集 3:打坐 4:双修打坐 5:剧情中 6:帮会席位中 7:婚礼拜堂 8:婚礼游行)"}
    ,{uint16, speed, "移动速度"}
    ,{uint32, hp_max, "气血上限"}
    ,{uint32, hp, "气血"}
    ,{uint16, x, "坐标x"}
    ,{uint16, y, "坐标y"}
    ,{uint16, gx, "所在格子坐标x"}
    ,{uint16, gy, "所在格子坐标y"}
).