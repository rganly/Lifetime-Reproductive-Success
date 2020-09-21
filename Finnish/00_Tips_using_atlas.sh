## This script list basic codes for FIMM atlas

#################
#      Job      #
#################

# submiting job
grun -n MyRJob -m e -M aoxing.liu@helsinki.fi -q highmem.q -mem 2 GB Rscript --vanilla  0_Files_info.R     # type "grun" from the terminal for full description  
qsub job.sh      # type "qsub" from the terminal for full description, see "/homes/aliu/DSGE_LRS/script/0_Files_info.sh" for example of script


# checking job status
qstat
status
qstat -g c       # check the availability of the queues


# cancel submitted job
qdel your_job_id




########################
#  Working environemnt #
########################

# activate conda environment 
source $HOME/.bashrc  &&  conda activate r_env



#######################
#    Share data       #
#######################

# change group 
chgrp sg_dsge_lrs file_name


# change permission
chmod 770 file_name





#################
#      R        #       
#################

# using interactive R
qrsh -q interactive.q              # requiring interactive sessions on cluster
/homes/aliu/DSGE_LRS/software/R    # R 3.6 with conda environment, many packages have already installed and could be directly accessed (e.g. library(ggplot2) ) 
/homes/aliu/DSGE_LRS/software/Rscript   # Rscript with conda environment



