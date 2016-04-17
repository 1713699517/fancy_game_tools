#! /usr/bin/env python
#-*- encoding: utf-8 -*-
## 数据转换配置

# ----------------------------------------------------------------
# 后端数据生成配置
# 详细配置方法示例请执行: $> ./cfg_svr.py
# 其中函数生成类型包括：
# "type": "single"      => 生成单值
# "type": "tuple"       => 生成tuple数据
# "type": "list"        => 生成list数据
# "type": "record.xxx"  => 生成record数据
# ----------------------------------------------------------------
svr_cfg = {
        # 地图
        "map_data" : {
            "include" : ["common.hrl", "map.hrl"],
            "function" : [
                {
                    "sheet": "base", "func": "all", "type": "list.single",
                    "keys": [],
                    "vals": ["id"]
                    },
                {
                    "sheet": "startup", "func": "startup", "type": "list.single",
                    "keys": [],
                    "vals": ["id"]
                    },
                {
                    "sheet": "base", "func": "get", "type": "record.map_data",
                    "keys": ["id"],
                    "vals": ["id", "name", "width", "height", "revive"]
                    },
                ]
            }

        }

      

#############################################################
## 以下是生成函数，请勿修改
#############################################################

def help():
    print '''
===========================================
后端数据生成配置col_svr.py填写示例格式：
===========================================
# 属性表
"attr_data" : {
    # 模块包含头文件
    "include" : ["attr.hrl"],
    # 定义函数接口生成规则
    "function" : [
        # 成长系数
        {
            # "sheet": "所选sheet", "func": "函数名", "type": "函数生成格式类型"
            "sheet": "grow_attr", "func": "get_grow", "type": "record.attr",
            # "keys": "函数参数对应的表头字段"
            "keys": ["career"],
            # "vals": "函数返回值对应的表头字段"
            "vals": ["hp_max", "mp_max", "speed", "atk"]
            },
        # 职业系数
        {
            "sheet": "career_attr", "func": "get_career", "type": "record.attr",
            "keys": ["career"],
            "vals": ["hp_max", "mp_max", "speed", "atk"]
            }
        ]
    }
===========================================
生成结果:
===========================================
数据表：attr_data
文件名：attr_data.erl
函数：
get_grow(Career) -> #attr{
    hp_max = 0
    ,mp_max = 0
    ,speed = 0
    ,atk = 0
}

===========================================
格式说明:
===========================================
    '''
    keys_help()
    vals_help()
    type_help()
    print '''

! 请按照以上规则配置或修改该cfg_svr.py文件，达到特定的数据生成格式
! 如果有还不满足的需求，请说明或自行修改
'''


# 键配置格式
def keys_help():
    print '''
## 配置keys参考示例:
配置格式                    生成格式
[]                      =>  get()
[\"base_id\"]           =>  get(BaseId)
[\"base_id\", \"lev\"]  =>  get({BaseId, Lev})
    '''

def vals_help():
    print '''
## 配置vals参考示例:
配置格式                    生成格式
[\"base_id\"]           =>  BaseId;
[\"base_id\", \"lev\"]  =>  {BaseId, Lev};
    '''
def type_help(arg=""):
    print '''
## 配置type参考示例:
配置格式                    生成格式
\"single\"              =>  Val;
\"tuple\"               =>  {Val, ...};
\"list\"                =>  [Val, ...];
\"maps\"                =>  #{Key => Val, ...};
\"record.item_base\"    =>  #item_base{...};
\"list.tuple\"          =>  [{...}, ...];
\"list.maps\"           =>  [#{...}, ...];
\"list.record.item_base\" => [#item_base{...}, ...];

    '''
    print arg

# 判断字符串是否为空
def is_space(s):
    return s.strip()==''

# 数据转换为特定项目代码结构
def convert_list(text):
    if text.find("\n") == -1:
        if text.find("|") == -1:
            return "[%s]" % convert_gainloss(text);
        else:
            sl = text.split("|")
            s_l = []
            for j in range(len(sl)):
                s = convert_gainloss(sl[j])
                if not is_space(s):
                    s_l.append(s)
            return "[%s]" % ", ".join(s_l);
    else:
        sl = text.split("\n")
        s_l = []
        for j in range(len(sl)):
            s = convert_gainloss(sl[j])
            if not is_space(s):
                s_l.append(s)
        return "[%s]" % ", ".join(s_l);
def convert_gainloss(text):
    try:
        if text.find(u'获得') >= 0 or text.find(u'失去') >= 0  or text.find(u'扣除') >= 0:
            tl = text.split(";")
            if len(tl) == 3:
                [gl, label, val] = text.split(";")
                if gl == u'获得':
                    return "#gain{label = %s, val = %s}" % (convert_gain_label(label), val)
                elif gl == u'失去' or gl == u'扣除':
                    return "#loss{label = %s, val = %s}" % (convert_loss_label(label), val)
                else:
                    pass
            elif len(tl) == 4:
                [gl, label, val, msg] = text.split(";")
                if gl == u'获得':
                    return "#gain{label = %s, val = %s, msg = %s}" % (convert_gain_label(label), val, msg)
                elif gl == u'失去' or gl == u'扣除':
                    return "#loss{label = %s, val = %s, msg = %s}" % (convert_loss_label(label), val, msg)
                else:
                    pass
        return text;
    except:
        print "损益数据填写有误: %s" % text
        exit(1)

def convert_gain_label(label):
    if label == u'元宝':
        return "gold";
    if label == u'经验':
        return "exp";
    if label == u'宠物经验':
        return "exp_pet";
    if label == u'铜钱':
        return "coin";
    if label == u'绑定铜钱' or label == u'绑铜钱':
        return "coin_bind";
    if label == u'副本次数':
        return "dung_times";
    if label == u'物品':
        return "item";
    if label == u'阅历':
        return "attainment";
    else:
        print "未识别的增益label: %s" % label
        return label
def convert_loss_label(label):
    if label == u'元宝':
        return "gold";
    if label == u'经验':
        return "exp";
    if label == u'铜钱':
        return "coin";
    if label == u'绑定铜钱' or label == u'绑铜钱':
        return "coin_all";
    if label == u'副本次数':
        return "dung_times";
    if label == u'阅历':
        return "attainment";
    if label == u'物品':
        # 按照基础ID删除: 
        # 失去;物品;BaseId
        # 失去;物品;{BaseId,Num}
        return "item_base_id";
    else:
        print "未识别的损益label: %s" % label
        return label

def list2str(l):
    if len(l) == 0:
        return ""
    if len(l) > 1:
        sl = []
        for x in l:
            sl.append("%s" % x)
        return "%s" % ','.join(sl)
    else:
        return "%s" % l[0]

# 生成函数名
def gen_fun_name(fun_name, key):
    if len(key) == 0:
        s = u"%s() ->" % fun_name;
    elif len(key) == 1:
        s = u"%s(%s) ->" % (fun_name, key[0]);
    else:
        s = u"%s({%s}) ->" % (fun_name, list2str(key))
    return s

# 生成函数值
def gen_fun_val(fun_types, fun_vals, val):
    s = " "
    if fun_types[0] == 'record':
        sl = []
        for i in range(len(fun_vals)):
            sl.append("%s = %s" % (fun_vals[i], val[i]))
        s += "#%s{\n\t%s}" % (fun_types[1], '\n\t,'.join(sl))
    elif fun_types[0] == 'maps':
        sl = []
        for i in range(len(fun_vals)):
            sl.append("%s => %s" % (fun_vals[i], val[i]))
        s += "#{\n\t%s}" % '\n\t,'.join(sl)
    elif fun_types[0] == 'single' or fun_types[0] == '':
        s += "%s" % val[0]
    elif fun_types[0] == 'tuple':
        s += "{%s}" % list2str(val)
    elif fun_types[0] == 'list':
        s += "[%s]" % list2str(val)
    else:
        print val
        type_help("暂时不支持的数据类型: %s" % fun_types)
    return s

# --------------------------------------------------------
# 类型一 的函数生成
# 适用：get(K) -> V. 其中K -> V 是一一对应的值类型
def gen_fun_11(fun_name, fun_types, fun_vals, key, val):
    s = gen_fun_name(fun_name, key)
    s += gen_fun_val(fun_types, fun_vals, val) 
    return s 

# --------------------------------------------------------
# 类型二的函数处理
# 适用 get(TypeId) -> [ TypeData1, TypeData2, ...] 一对多的关系
# 适用 get({TypeId1, TypeId2}) -> [ TypeData1, TypeData2, ...] 一对多的关系
def gen_fun_1n(fun_name, fun_types, fun_keys, fun_vals, keys, vals):
    keylen = len(fun_keys)
    k_vs = {} # {K1: [K1V1, K1V2, ...], K2: [K2V1, K2V2, ...]}
    # 汇总数据
    if len(keys) <= 0 or len(vals) <= 0:
        return "";
    else:
        k_vs[list2str(keys[0])] = [0]
        for i in range(1, len(keys)):
            k = list2str(keys[i])
            if k_vs.has_key(k) :
                # 记录相同键的索引序号列表
                li = k_vs[k]
                li.append(i)
                k_vs[k] = li
            else:
                k_vs[k] = [i]
        sfun = ""
        for k,v in k_vs.items():
            sl = []
            s = gen_fun_name(fun_name, k.split(','))
            for j in v:
                sl.append(gen_fun_val(fun_types, fun_vals, vals[j]))
            s += "[\n%s\n]%s" % ((',\n'.join(sl)), symbol(fun_keys))
            sfun += s
        return sfun

# ----------------------------------------
# 符号
def symbol(fun_keys):
    if len(fun_keys) == 0:
        return ".";
    else:
        return ";\n"

# ---------------------------------------------------------
# 容错匹配函数的生成
def gen_other_key(fun_name, fun_keys):
    if len(fun_keys) == 0:
        return ""
    elif len(fun_keys) == 1:
        return "%s(_) -> " % fun_name
    else:
        s_l = []
        for i in range(len(fun_keys)):
            s_l.append("_")
        return "%s({%s}) -> " % (fun_name, ",".join(s_l))
def gen_other_val(fun):
    if fun.has_key("ignore"):
        return "%s." % fun["ignore"];
    elif fun["keys"] == []:
        return ""
    else:
        fun_types = fun["type"].split(".")
        if fun_types[0] == 'list' :
            return "[].";
        else:
            return "error."


# 处理函数生成
def deal_func(fun, keys, vals):
    fun_name = fun["func"]
    fun_type = fun["type"]
    fun_keys = fun["keys"]
    fun_vals = fun["vals"]
    doc = ""
    if fun.has_key("desc"):
        doc += "%% "
        doc += "%s\n" % fun["desc"]
    # 拆分fun_type，区分不同函数类型生成方式
    fun_type_list = fun_type.split(".")
    if len(fun_type_list) == 1:
        for i in range(len(keys)): # 以key列表作为行数
            key = keys[i]
            doc += gen_fun_11(fun_name, fun_type_list, fun_vals, key, vals[i])
            doc += symbol(fun_keys)
    elif len(fun_type_list) == 2:
        if fun_type_list[0] == "record":
            for i in range(len(keys)):
                key = keys[i]
                doc += gen_fun_11(fun_name, fun_type_list, fun_vals, key, vals[i])
                doc += ";\n"
        elif fun_type_list[0] == "list":
            # list.single
            # list.tuple
            # list.maps
            doc += gen_fun_1n(fun_name, [fun_type_list[1]], fun_keys, fun_vals, keys, vals)
        else:
            type_help("暂时不支持的函数生成类型: %s" % fun_type)
    elif len(fun_type_list) == 3:
        if fun_type_list[0] == "list":
            # list.record.xxx
            doc += gen_fun_1n(fun_name, fun_type_list[1:], fun_keys, fun_vals, keys, vals)
        else:
            type_help("暂时不支持的函数生成类型: %s" % fun_type)
    else:
        type_help("#暂时不支持的函数生成类型: %s" % fun_type)
    doc += gen_other_key(fun_name, fun_keys)
    doc += gen_other_val(fun)
    # print(doc)
    return doc
        
# help()
