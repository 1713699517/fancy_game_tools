#! /usr/bin/env python
#-*- encoding: utf-8 -*-
import sys, os, string, struct
import xlrd
from cfg_svr import ( svr_cfg, deal_func)
import cfg_svr
import cfg_path
import pprint
reload(sys)
sys.setdefaultencoding( "utf-8" )

def help():
    print """===========================
命令示例:
./gen.py all            默认生成zh_cn版本的所有数据
./gen.py all zh_cn      生成zh_cn版本的所有数据
./gen.py xxx_data       默认生成zh_cn版本 xxx_data 数据
./gen.py xxx_data zh_cn 生成zh_cn版本 xxx_data 数据
==========================="""
    cfg_svr.help()
    exit(1)

# 过滤原始值(xlsx提取的表格值，处理1 => 1.0这种情况)
def filter_value(value):
    if isinstance(value, float):
        s = "%s" % value
        sf = s.split('.')[1]
        if int(sf) == 0:
            return int(s.split('.')[0])
        else:
            return value
    return value

# 转为string
def to_str(value):
    return "%s" % filter_value(value)
# 转换整形值
def to_int(value):
    if value == '':
        return 0
    elif type(value) == str:
        return int(value.split(':')[0])
    elif type(value) == unicode:
        return int(value.split(':')[0])
    else:
        return int(value)
# 转换浮点值
def to_float(value):
    if value == '':
        return 0
    else:
        return filter_value(value)
# 转换为原子
def to_atom(value):
    if value == '':
        return 'undefined'
    else:
        return "\'%s\'" % value.split(':')[0]

# 根据表头限定检查行数据，转换为对应的代码可识别的数据格式
def check_row(data_type, heads, row):
    if len(heads) == len(row):
        if data_type == "svr":
            for i in range(len(heads)):
                head = heads[i]
                if head["type"] == "int":
                    value = row.pop(i)
                    row.insert(i, to_int(value))
                elif head["type"] == 'float':
                    value = row.pop(i)
                    row.insert(i, to_float(value))
                elif head["type"] == 'string':
                    # Erlang代码的bitstring格式
                    value = row.pop(i)
                    if head["svr"] == "label_res" or head["svr"] == "cont_desc" or head["svr"] == "time_desc" or head["svr"] == "cond_desc":
                        row.insert(i, "\"%s\"" % value)
                    else:
                        row.insert(i, "<<\"%s\"/utf8>>" % value)
                elif head["type"] == 'tuple':
                    # Erlang代码的tuple格式
                    value = row.pop(i)
                    row.insert(i, "%s" % to_str(value))
                elif head["type"] == 'atom':
                    # Erlang代码的atom格式
                    value = row.pop(i)
                    row.insert(i, to_atom(value))
                elif head["type"] == 'term':
                    # Erlang代码的term格式，默认加上[]符号
                    value = to_str(row.pop(i))
                    row.insert(i, cfg_svr.convert_list(value))
                else:
                    print "## 表头[%s]类型[%s]定义异常" % (head["svr"], head["type"])
                    exit(1)
            return row
        else:
            for i in range(len(heads)):
                head = heads[i]
                if head["type"] == 'int':
                    value = row.pop(i)
                    row.insert(i, to_int(value))
                elif head["type"] == 'float':
                    value = row.pop(i)
                    row.insert(i, str(value))
                else :
                    value = row.pop(i)
                    row.insert(i, value)
            return row
    else:
        print len(heads)
        print len(row), row
        print "## 表头类型定义与xlsx表数量不符合"
        exit(1)

# 获取表头配置，生成字典
def get_colhead(name, sheet):
    heads = []
    try:
        cols = col_cfg[name][sheet]
        for i in range(0, len(cols)):
            head = {}
            if len(list(cols[i])) == 4:
                (colname, cli, svr, dtype) = cols[i]
                head["idx"] = i
                head["col"] = colname
                head["cli"] = cli
                head["svr"] = svr
                head["type"] = dtype
            else:
                (cli, svr, dtype) = cols[i]
                head["idx"] = i
                head["cli"] = cli
                head["svr"] = svr
                head["type"] = dtype
            heads.append(head)
        return heads
    except :
        print "未在表[%s]找到表头配置[%s]，请检查col_cfg" % (name, sheet)
        exit(1)

# 提取对应的工作表数据
def find_table(book, sname, heads, datatype):
    has_table = 0
    for sheet in book.sheets():
        if sheet.name == sname:
            table = sheet
            has_table = 1
            break
        else:
            if sheet.name.find("#") == -1:
                continue;
            if sheet.name.split("#")[1] == sname:
                table = sheet
                has_table = 1
                break
            else:
                continue
    if has_table == 0:
        print "工作表未找到或为空:%s" % sname
        exit(1)
    if table.nrows <= 0 or table.ncols <= 0:
        print "工作表未找到或为空:%s" % sname
        exit(1)
    # 数据转成 [[Col1, Col2, ...], ...]
    rows = []
    # 去掉表头一行
    # row_head = table.row_values(0)
    # 提取实际数据行，并转换数据类型
    for i in range(1, table.nrows):
        row = table.row_values(i)
        rows.append(check_row(datatype, heads, row))
    return rows

# 根据键列表查询索引，返回索引列表
def keys_to_idx(keys, heads):
    idxlist = []
    for key in keys:
        for head in heads:
            if head["svr"] == key:
                idx = head["idx"]
                break
            else:
                continue
        try:
            idxlist.append(idx)
        except NameError:
            print "搜索表头未找到配置的键%s" % key
            print key
            print heads
            exit(1)
    return idxlist

# 根据索引取一组row值，返回列表rows
def idx_to_rows(idxlist, rows):
    vals = []
    for row in rows:
        tmp = []
        for idx in idxlist:
            tmp.append(row[idx])
        vals.append(tmp)
    return vals

# 获取类名
def class_name(name, sname):
    return camel_casing("%s_%s" % (name, sname))

# 转换驼峰式类名
def camel_casing(oriStr, splitStr = "_"):
    str_list = oriStr.split(splitStr)
    if len(str_list) > 1:
        for index in range(0, len(str_list)):
            if str_list[index] != '':
                str_list[index] = str_list[index][0].upper() + str_list[index][1:]
            else:
                continue
        return ''.join(str_list)
    else:
        return oriStr
        
# 生成指定数据
def gen_data(name, genFor=0):
    if not os.path.isfile(path["xlsx"] + name + ".xlsx"):
        print "\n### 文件[%s.xlsx]不存在\n" % name
        help()
    print ">>==== 开始生成[%s]..." % name
    # 打开文件
    book = xlrd.open_workbook(path["xlsx"] + name + ".xlsx")
    gen_col_cfg(book, name)
    if genFor == 0:
        gen_erl(book, name)
        gen_cli_2(book, name)
    elif genFor == 1:
        gen_erl(book, name)
    elif genFor == 2:
        gen_cli_2(book, name)
    else:
        raise AttributeError("生成文件参数错误, 只能0,1,2")

    print ">>==== [%s]生成ok~!\n" % name
    return

# 获取生成表头配置
def gen_col_cfg(book, name):
    col_cfg[name] = {}
    for sheet in book.sheets():
        cols = []
        if sheet.name.find("#") == -1:
            continue;
        [_, sname] = sheet.name.split("#")
        if sheet.nrows <= 0 or sheet.ncols <= 0:
            print "工作表sheet为空: %s" % sheet.name
            exit(1)
        for col in sheet.row_values(0):
            if isinstance(col, float):
                cols.append((col, "none", "none", "string"));
            elif col.find("#") == -1:
                cols.append((col, "none", "none", "string"))
            else:
                try:
                    (cname, tmp) = col.split("#")
                    (cn,sn,st) = tmp.split(":")
                    cols.append((cname, cn, sn, st))
                    is_none_sheet = 1
                except:
                    print "工作表[%s]字段[%s]填写格式错误" % (sheet.name, col)
                    exit(1)
        col_cfg[name][sname] = cols

# 遍历col_cfg，检查是否没有client的数据需要生成
def check_gen_cli(cfg):
    is_need_gen = 0
    for k in cfg.keys():
        for (n,c,s,t) in cfg[k]:
            if c == "none":
                continue;
            else:
                is_need_gen = 1
    return is_need_gen;

# 生成指定的客户端数据
def gen_cli(book, name):
    if col_cfg[name] == {}:
        print "## %s表不需要生成cli数据" % name
        return;
    if check_gen_cli(col_cfg[name]) == 0:
        print "## %s表不需要生成cli数据" % name
        return;
    luaFile = "%s%s.lua" % (path["cli"], name)
    datFile = "%s%s.dat" % (path["cli"], name)
    lua = open(luaFile, 'w+')
    dat = open(datFile, 'wb+')
    # 生成lua文件
    lua.write("require(\"ProtoBase\")\n\n")
    luaStr = ""
    for sname in col_cfg[name]:
        heads = get_colhead(name, sname)
        s = gen_cli_lua(name, sname, heads)
        luaStr += s
    lua.write(luaStr)
    lua.flush()
    print "生成lua文件: %s" % luaFile
    # 生成dat文件，dat文件是合并的
    # 格式：TableName + numSheet + SheetName1 + ClassName1 + SheetName1_DocLen + SheetRowsNum + SheetName1_BinCont + SheetName2...
    numSheet = len(col_cfg[name])
    dat.write(to_bin('string', name))
    dat.write(to_bin('int', numSheet))
    pp = pprint.prettyPrinter(depth=4)
    for sname in col_cfg[name]:
        heads = get_colhead(name, sname)
        rows = find_table(book, sname, heads, "cli")
        (contLen, Bin) = gen_sheet_dat(rows, sname, heads)
        dat.write(to_bin('string', sname)) # 前端用作查询键
        dat.write(to_bin('string', class_name(name, sname))) # 子表解析类名
        dat.write(to_bin('int', contLen)) # 字表数据长度，包括4bytes的rowLen值
        dat.write(Bin)
        print numSheet,sname, class_name(name, sname)
        # print "===contlen: %s" % contLen
        # print repr(Bin)
    dat.flush()
    print "生成dat文件: %s" % datFile

# 生成dat二进制数据文件
def gen_sheet_dat(rows, sname, heads):
    rLen = 0 # 数据行数
    cLen = 0 # 数据总字节长度
    b = bytes('')
    for row in rows:
        for i in range(len(row)):
            if heads[i]["cli"] == "none":
                continue;
            elif heads[i]["cli"] == "":
                continue;
            else:
                b += to_bin(heads[i]["type"], filter_value(row[i]))
                cLen = len(b)
        if cLen <= 0:
            print "数据[%s]无需生成dat跳过..." % sname
            break; # 在此处跳出循环，不再遍历所有行
        rLen += 1
    cLen += 4
    b = to_bin('int', rLen) + b
    print rLen,cLen
    return cLen, b

# TODO 生成lua文件，其中heads的格式参考get_colhead
def gen_cli_lua(name, sname, heads):
    ClassName = class_name(name, sname)
    luaStr = ""
    luaStr += "%s = class(\"%s\", ProtoBaseUnpack)\n" % (ClassName, ClassName)
    luaStr += """
function %s:ctor()
    %s.super.ctor(self)
    self.list = {}
end
function %s:unpack(bytes, offset)
    offset = offset or 1
    self._byteData:writeBytes(bytes, offset, bytes:getAvailable()):setPos(1)
    self:fill(self._byteData)
end
function %s:fill(_ba)
    local list_len = _ba:readUInt()
    for i=1,list_len do
        local v = %s_list.new()
        v:fill(_ba)
        table.insert(self.list, v)
    end
end

%s_list = class("%s_list")
""" % (ClassName, ClassName, ClassName, ClassName, ClassName, ClassName, ClassName)
    ClassName += "_list"
    StrConstructor = ""
    StrFill = ""
    # 生成构造初始值，读取方法的填充字符))
    for head in heads:
        if head["cli"] == "none":
            continue;
        elif head["cli"] == "":
            continue;
        else:
            if head["type"] == "int":
                StrConstructor += "\n    self.%s = %s" % (head["cli"], 0)
                StrFill += "\n    self.%s = _ba:readUInt()" % head["cli"]
            else:
                StrConstructor += "\n    self.%s = %s" % (head["cli"], "\"\"")
                StrFill += "\n    self.%s = _ba:readStringUInt()" % head["cli"]
    FuncCtor = """
function %s:ctor()%s
end
""" % (ClassName, StrConstructor)
    FuncFill = """
function %s:fill(_ba)%s
end
""" % (ClassName, StrFill)
    luaStr += FuncCtor
    luaStr += FuncFill
    return luaStr + "\n\n"

# 写二进制数据(客户端)
# TODO 采用网络字节序(big-endian)
def to_bin(dtype, data):
    if dtype == 'int':
        return struct.pack('!i', data)
    elif isinstance(data, unicode) or isinstance(data, str) or dtype == 'string' or dtype == 'term' or dtype == 'atom' :
        s = unicode(data)
        b = bytes(s.decode('utf-8'))
        len_b = len(b)
        ba = struct.pack('!i%ss' % len_b, len_b, b)
        # print "==s: %s" % s # 打印测试
        # print "==b: %s  ba: %s" % (len(b), len(ba)) # 打印测试
        return ba
    else:
        print "遇到前端无法解析的数据格式[%s]错误: %s" % (dtype, data)
        exit(1)

def gen_cli_2(book, name):
    if col_cfg[name] == {}:
        print "## %s表不需要生成cli数据" % name
        return;
    if check_gen_cli(col_cfg[name]) == 0:
        print "## %s表不需要生成cli数据" % name
        return;
    lua_file = "%s%s.lua" % (path["cli"], name)
    with open(lua_file, "w+") as f:
        f.write("local %s = {}\n"%name)
        f.write("local item\n")
        for sname in col_cfg[name]:
            list_name = "%sList"%sname
            f.write("local %sList = {}\n"%sname)
            f.write("%s[\"%s\"] = %sList\n"%(name, sname, sname))
            heads = get_colhead(name, sname)
            rows = find_table(book, sname, heads, "cli")
            cn = class_name(name, sname)
            gen_sheet_instance(heads, rows, cn, list_name, f)
        f.write("return %s\n"%name)

    print "生成lua文件: %s" % lua_file

# 生成表实例
def gen_sheet_instance(heads, rows, cn, list_name, f):

    if len(rows) == 0:
        return

    key_dict = {}
    row = rows[0]
    for i, value in enumerate(row):
        head = heads[i]
        if head["cli"] == "none" or head["cli"] == "":
            continue
        key_dict[i] = head

    if len(key_dict) == 0:
        print "数据[%s]无需生成dat跳过..." % cn
        return

    f.write("\n")

    # item_name = "%sItem"%cn
    # f.write("%s = class(\"%s\")\n"%(item_name, item_name))
    is_empty = True
    for index, row in enumerate(rows, 1):
        # f.write("item = %s:create()\n"%item_name)
        f.write("item = {}\n")
        for k, v in key_dict.items():
            fv = filter_value_cli(v["type"], row[k])
            f.write("item.%s = %s\n"%(v["cli"], fv))
        f.write("%s[%d] = item\n"%(list_name, index))

    f.write("\n\n")

    pass

def filter_value_cli(t, value):

    # #先处理excel生成回来的浮点数
    # if isinstance(value, float):
    #     return int(value)

    if t == 'string':
        value = to_str(value)

    if t != 'int' and t != 'float':
        value = to_str(value)

    if isinstance(value, str) or isinstance(value, unicode):
        if len(value) == 0:
            return "\"\""
        value = value.replace("\"", "\\\"")
        return "\"%s\""%value

    return value

# 生成erl文件
def gen_erl(book, name):
    if not svr_cfg.has_key(name):
        print "## %s表不需要生成erl数据" % name
        return;
    if not os.path.isdir(path["svr"]):
        print "## svr目录不存在跳过生成erl数据: %s" % path["svr"]
        return;
    cfg = svr_cfg[name]
    erl = open("%s%s.erl" % (path["svr"], name), 'w+') # 打开erl
    mod = name
    hrls = cfg["include"]
    funs = cfg["function"]
    # 生成erl文件的头
    erl.write('''%%==================================
%% @doc 由工具生成的数据文件，不需要手动修改
%%==================================\n''')
    erl.write("-module(%s).\n" % mod)
    erl.write("-compile(export_all).\n")
    for hrl in hrls:
        erl.write("-include(\"%s\").\n" % hrl)
    erl.write("\n")
    # 生成erl文件的函数内容
    for fun in funs:
        if isinstance(fun["sheet"], list): # 支持多表合并，要求表头格式必须一致
            heads = get_colhead(name, fun["sheet"][0])
            rows = []
            for i_sheet in fun["sheet"]:
                rows.extend(find_table(book, i_sheet, heads, "svr"))
        else:
            heads = get_colhead(name, fun["sheet"])
            rows = find_table(book, fun["sheet"], heads, "svr")
        # 查找标签对应的列索引(因为表头字段格式复杂，先按照record的定义字段找出索引，方便访问)
        key_idx = keys_to_idx(fun["keys"], heads)
        val_idx = keys_to_idx(fun["vals"], heads)
        to_keys = idx_to_rows(key_idx, rows)
        to_rows = idx_to_rows(val_idx, rows)
        doc = deal_func(fun, to_keys, to_rows)
        erl.write(doc)
        erl.write("\n\n")
        print "生成erl函数:%s" % fun["func"]
    erl.flush()

#本地生成调用
def gen_native():
    # ----------------------------------------------------------------
    # 要生成的数据标签，比如: item_data
    if len(sys.argv) < 2:
        help()
    if len(sys.argv) > 4:
        help()
    if len(sys.argv) == 2:
        gLang = 'zh_cn'
    if len(sys.argv) == 3:
        gLang = sys.argv[2]
    # ----------------------------------------------------------------
    # 获取规则配置
    gName = sys.argv[1]

    global path, col_cfg
    path = cfg_path.path[gLang]

    if gName == "all":
        files = os.listdir(path["xlsx"])
        for f in files:
            if os.path.splitext(f)[1] == '.xlsx':
                col_cfg = {}
                gen_data(os.path.splitext(f)[0])
    else:
        col_cfg = {}
        gen_data(gName)

#web生成调用
def gen_web():
    # ----------------------------------------------------------------
    # 要生成的数据标签，比如: item_data
    if len(sys.argv) < 4:
        help()
    if len(sys.argv) > 6:
        help()
    if len(sys.argv) == 4:
        gLang = 'zh_cn'
        gUser = sys.argv[2]
    if len(sys.argv) == 5:
        gLang = sys.argv[2]
        gUser = sys.argv[3]
    # ----------------------------------------------------------------
    # 获取规则配置
    gName = sys.argv[1]
    global path, col_cfg
    path = {'xlsx': './' + gUser + '/',
            "cli": './' + gUser + '/'}

    col_cfg = {}
    gPath = gName.split('.')[1]
    PArr = gPath.split('/')
    gen_data(PArr[2], 2)


def main():

    args = len(sys.argv)
    if args == 1:
        help()

    is_gen_web = False
    for arg in sys.argv:
        if arg == "web":
            is_gen_web = True
            break

    if is_gen_web:
        gen_web()
    else:
        gen_native()

if __name__ == '__main__':
    main()
