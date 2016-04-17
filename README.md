#fancy_game_tools


#需要安装的库
————————————————————————

1) Python-2.7.X
2) openyxl.zip
3) xlrd-0.9.3.tar.gz



#数据生成工具使用帮助
————————————————————————

## 工具配置
### 路径配置
  *文件：cfg_path.py
  *作用：定义不同版本的数据生成相关的目录
### 表头配置
  *在excel表里面配置，通过表头格式定义
### 后端配置
  *文件：cfg_svr.py
  *作用：根据模板定义的函数接口，生成erl数据源码文件
  *帮助：./cfg_svr.py

## 命令生成
### 示例1：
* $> ./gen.py item_data zh_cn
### 示例2：
* $> ./gen.py item_data zh_tw
