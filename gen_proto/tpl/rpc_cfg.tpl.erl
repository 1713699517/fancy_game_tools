%%----------------------------------------------------
%% 客户端RPC调用配置
%% 此文件由程序生成，不要手动修改
%%----------------------------------------------------
-module(rpc_cfg).
-export([
        get/1
        ,desc_fun/2
    ]
).

{{rpc}}get(Code) ->
    {error, {rpc_cfg_undefined, Code}}.

{{desc_fun}}desc_fun(_, _) -> undefined.
