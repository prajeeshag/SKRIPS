
wrfoutFile='wrfout_d01_2000-05-30_00:00:00'

ncdump -h $wrfoutFile | grep float | grep bottom | grep south | sed 's/(/ /g' | awk '{print $2}' > wrfout3dvars.log
ncdump -h $wrfoutFile | grep float | grep -v bottom | grep south | grep west | sed 's/(/ /g' | awk '{print $2}' > wrfout2dvars.log
ncdump -h $wrfoutFile | grep float | sed 's/(/ /g' | awk '{print $2}' > wrfoutvars.log
ncdump -h $wrfoutFile | grep int | sed 's/(/ /g' | awk '{print $2}' > wrfoutvars.log
