
#############################
#  Upload or download files #
#############################

## upload files to bianca
scp lrs_r3.zip  aoxing@rackham.uppmax.uu.se:/home/aoxing/lrs/input            # from local terminal copy file from local to rackham
ssh -A aoxing@rackham.uppmax.uu.se                                            # login rackham
sftp -q aoxing-sens2019018@bianca-sftp.uppmax.uu.se:aoxing-sens2019018        # login the sftp prompt of bianca
put lrs_r3.zip                                                                # upload in the sftp prompt
cp /proj/sens2019018/nobackup/wharf/aoxing/aoxing-sens2019018/lrs_r3.zip   /proj/sens2019018/SCB_data/release3   # from bianca terminal copy files to the target directory



## download files from bianca
cp /proj/sens2019018/socialstyrelsen/Variabellista_27035_2018.xlsx  /proj/sens2019018/nobackup/wharf/aoxing/aoxing-sens2019018   # from bianca termianl copy files from bianca to wharf, since only wharf has internet connection due to the security reason
ssh -A aoxing@rackham.uppmax.uu.se                                            # login rackham in another window
sftp -q aoxing-sens2019018@bianca-sftp.uppmax.uu.se:aoxing-sens2019018        # login the sftp prompt of bianca
get Variabellista_27035_2018.xlsx                                             # download in the sftp prompt
scp aoxing@rackham.uppmax.uu.se:/domus/h1/aoxing/Variabellista_27035_2018.xlsx  /Users/aoxliu/Documents/LRS/Data/SWE/release4 



## Uncompress files in bianca
cd /proj/sens2019018/SCB_data/release3
jar xvf  lrs_r3.zip                               
module load p7zip
7z e  Tove_Lev_LISA_1977_2017.exe && 7z e  Tove_Lev_Ovriga_tabeller.exe && 7z e  Tove_Lev_RTB_1977_2017.exe                    




#############################
#  Using interactive queue  #
#############################

## Using interactive queue
interactive -A sens2019018 -n 6 --qos=interact -t 12:00:00    # ask for 6 nodes for 12 hours
jobinfo -u aoxing
bianca_combined_jobinfo     # check the queues for all projects in Bianca 



# Using R 
module load R/4.0.0
module load R_packages/4.0.0
R




