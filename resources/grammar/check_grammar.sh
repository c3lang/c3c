echo "Testing lib"
for fn in `find ../../../c3c/lib -name '*.c3' | sort`
do
     echo -n $fn
     cat $fn | ./c3yacc
done
echo "Testing resources"
for fn in `find ../../../c3c/resources -name '*.c3' | sort`
do
     echo -n $fn
     cat $fn | ./c3yacc
done
echo "Testing unit tests"
for fn in `find ../../../c3c/test/unit -name '*.c3' | sort`
do
     echo -n $fn
     cat $fn | ./c3yacc
done
echo "Testing compiler tests"
for fn in `find ../../../c3c/test/test_suite -name '*.c3t' | sort`
do
     echo -n $fn
     cat $fn | ./c3yacc
done
