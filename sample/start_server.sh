#!/bin/sh
# author TAKUYA watabe
#
# this sample startup script only tends to be executed at project root dir
# (just upper sample dir)
# ex  "./sample/start_server.sh"
basedir=`dirname $0`

debug=$1
if test  "-d" = $debug; then
    param="-i -T --conf $basedir/conf/yaws.conf"
else
    param="--daemon --conf $basedir/conf/yaws.conf"
fi
yaws $param
