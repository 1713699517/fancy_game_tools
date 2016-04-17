#/bin/bash
# ---------------------------------------------------------
# 协议编译
# ---------------------------------------------------------

fun_gen_all(){
    rm -rf ebin/*.beam
    erl +pc unicode -pa "../../server/ebin" -pa "./ebin" -eval "make:all()" -eval " io:setopts([{encoding,unicode}]), main:compile(all)" -s c q
}

fun_gen(){
    rm -rf ebin/*.beam
    erl +pc unicode -pa "../../server/ebin" -pa "./ebin" -eval "make:all()" -eval " io:setopts([{encoding,unicode}]), main:compile($1)" -s c q
}

if [ "$1" == "" ]; then
    fun_gen_all
    exit 0
fi
case $1 in
    all) fun_gen_all;;
    *) fun_gen $1;;
esac
echo 
exit 0;
