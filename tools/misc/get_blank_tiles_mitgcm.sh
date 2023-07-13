grep Empty STDOUT.* | sed -r 's .{47}  ' | sed 's/ (bi,bj=   1   1 )/, /g' | cut -c 1-8 > trash1.txt
cat trash1.txt | tr '\n' ' ' > blanks.txt
wc -l trash1.txt
