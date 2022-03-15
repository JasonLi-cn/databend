#!/usr/bin/env bash

CURDIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
. "$CURDIR"/../../../shell_env.sh


## create table t09_0012
echo "create table t09_0012(c int)" | $MYSQL_CLIENT_CONNECT
# there will be two blocks after two insertions
echo "insert into t09_0012 values(1)" | $MYSQL_CLIENT_CONNECT
echo "insert into t09_0012 values(2)" | $MYSQL_CLIENT_CONNECT

echo "explain select * from t09_0012" | mysql -h127.0.0.1 -P3307 -uroot -s
echo "explain select * from t09_0012 limit 1" | mysql -h127.0.0.1 -P3307 -uroot -s
echo "explain select * from t09_0012 limit 0" | mysql -h127.0.0.1 -P3307 -uroot -s

## Drop table.
echo "drop table  t09_0012" | $MYSQL_CLIENT_CONNECT
