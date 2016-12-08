#!/bin/bash
lines=`cat tests.lisp | wc -l`
echo '<testsuite tests="'$lines'">' > report.xml
i=1
while read test
do
	if [[ "$test" != "" ]] && [[ "$test" != \;* ]]; then
		res=`echo "$test" | sbcl --noinform --load mvpoli.lisp | sed -n 2p`
		if [ "$res" == "T" ]; then
                	echo '<testcase classname="test'$i'" name="test'$i'"/>' >> report.xml
        	else
                	echo '<testcase classname="test'$i'" name="test'$i'">' >> report.xml
			echo '<failure type="Fail"> '$res' </failure>' >> report.xml
			echo '</testcase>' >> report.xml
        	fi
	fi
	i=$((i+1))

done < tests.lisp
echo '</testsuite>' >> report.xml

