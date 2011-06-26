#!/bin/sh
basedir=`dirname $0`
debug=$1
if test  "-d" = $debug; then
    param="-i -T --conf $basedir/conf/yaws.conf"
else
    param="--daemon --conf $basedir/conf/yaws.conf"
fi
yaws $param
