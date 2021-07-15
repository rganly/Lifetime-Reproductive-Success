#!/bin/sh
#$ -S /bin/sh
#$ -N childless_main
#$ -l h_vmem=35G
#$ -l h_cpu=66:00:00
#$ -l h_rt=66:00:00
#$ -q hugemem.q
#$ -M aoxing.liu@helsinki.fi
#$ -m e
#$ -o /homes/aliu/DSGE_LRS/script/REGRESSION/13_REG_childless_main.OU
#$ -e /homes/aliu/DSGE_LRS/script/REGRESSION/13_REG_childless_main.ER

#copy local envirnmental variables
#$ -V

#cd to execution directory
#$ -cwd
cd /homes/aliu/DSGE_LRS/script/REGRESSION/


#Header info
echo "Run on cluster node: " $HOST
echo "Run directory: " `pwd`
echo "Start time: " `date`
echo "---------------------------------------"


#Increase stack size
export MPSTKZ=4500M
 ulimit -s unlimited


#Run command

/homes/aliu/DSGE_LRS/software/Rscript  --vanilla  13_REG_childless_main.R



