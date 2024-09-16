for fn in `find ../../../c3c -name '*.c3' | sort`
do
     echo -n $fn
     cat $fn | ./c3yacc
done
