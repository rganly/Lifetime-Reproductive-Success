########################################
#             login                    #
########################################

## Open Terminal and use ssh to login
ssh your_user_name@ssh.fimm.fi

## At first login, you will get the following message, which is for generating public/private rsa key:
It doesn't appear that you have set up your ssh key.
This process will make the files:
/homes/user_name/.ssh/id_rsa.pub
/homes/user_name/.ssh/id_rsa
/homes/user_name/.ssh/authorized_keys
Generating public/private rsa key pair.
Enter file in which to save the key (/homes/user_name/.ssh/id_rsa):
Just enter ‘yes’

# If you get the warning:
warning: setlocale: LC_CTYPE: cannot change locale (UTF-8): No such file or directory
Mac users go to: Terminal -> preferences -> profiles -> advanced -> uncheck ‘Set locale environment variables on startup’ -> restart terminal (otherwise, at least R cannot interpret special characters)
The LC_CTYPE is for character handling rules governing the interpretation of sequences of bytes of text data characters and the behavior of character classes.



########################################
#                 Job                  #
########################################

## Submitting job
# via grun
grun -n MyRJob -m e -M your_emaili -q highmem.q -mem 2 GB Rscript --vanilla  0_Files_info.R         # type "grun" from the terminal for full description     

# via qsub 
qsub job.sh       # type "qsub" from the terminal for full description, see "/homes/aliu/DSGE_LRS/script/0_Files_info.sh" for example of the .sh script

# Need to increase stack size by adding the following codes to the .sh file, when running large job using qsub
export MPSTKZ=2500M
ulimit -s unlimited


## checking job status
qstat  -g  c      # check the availability of the queues
qstat      # check the status of your job
status
qstat -j <job-id>|grep maxvmem   # To find the memory usage of your jobs run 

## cancel submitted job
qdel  your_job_id


## use Interactive sessions on the cluster
qrsh -q interactive.q      # Interactive sessions on the cluster can be queued, you can use this interactive queue to test your codes. If there is a free slot you will receive one within a few seconds.




#########################################
#         Working environment           #
#########################################

## activate conda environment (I used conda environment in order to install package without admin permission)
source $HOME/.bashrc  && conda create -n r_env      # create an conda environment
source $HOME/.bashrc  && conda activate r_env      # activate your conda environment



###################################################################
#    Share data to user of the same working group                 #
###################################################################

# change group (sg_dsge_lrs is the working group for “LRS” project)
chgrp sg_dsge_lrs file_name


# change permission
chmod 770 file_name      # 770 means the owner and group have full permissions (read, write, and execute), but everyone outside your working group has no permissions. (From left to right, the numbers refer to the owner, group and others.)



#####################################
#            Using  R               #
#####################################

## using interactive R to test your codes
qrsh -q interactive.q      # first require interactive sessions on cluster
/homes/aliu/DSGE_LRS/software/R      # then use R 3.6 with conda environment, many packages have already installed and could be directly accessed (e.g. library(ggplot2) ) 
source $HOME/.bashrc && conda activate r_env && conda install -c r r-hmisc      # install R-package for conda environment, use library(Hmisc, lib.loc="/homes/aliu/anaconda3/lib/R/library") when you want to use the newly installed package


 # Rscript with conda environment for your .sh file when you submit job using qsub
/homes/aliu/DSGE_LRS/software/Rscript     




################################################
#          Upload/download file                #
################################################

## download to local address
scp your_user_name@ssh.fimm.fi:/homes/address_at_atlas    your_local_taget_address


## upload to FIMM ATLAS
scp  your_local_address   your_user_name@ssh.fimm.fi:/homes/address_at_atlas 



###################################################
#           Basic Unix Command Line               #
###################################################

https://cfenollosa.com/misc/workshop_unix_cheat_sheet.pdf
https://cfenollosa.com/misc/workshop_unix.pdf
https://github.com/learnbyexample/Command-line-text-processing


