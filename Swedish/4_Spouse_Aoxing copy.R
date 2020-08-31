##Ask interactive nodes ###
interactive -A sens2019018 -n 1 --qos=interact -t 12:00:00
salloc -A sens2019018 -p core -n 1 -t 12:00:00
jobinfo -u aoxing
setwd("/home/aoxing/lrs/output/release3")

#========================================================================================================================================================================================================|
#1.7 spouse of indexperson or sibling
#1.7.1 spouse of indexperson 
for (year_n in 1998:2017){
	index_spouse_yearn<-get(load(paste("/proj/sens2019018/SCB_data/release3/r_files/tove_lev_koppl_index_part_",year_n,".Rdata",sep="")))              # LopNrSyskon LopNrSamh FodelseArSamh FodelseLanSamh FodelseKommunSamh KonSamh
	index_spouse_yearn[,"year_n"]<-year_n
	if (year_n==1998){
		index_spouse<-index_spouse_yearn
	}else{
		index_spouse<-rbind(index_spouse,index_spouse_yearn)
	}
	print(year_n)
}
#save(index_spouse,file="index_spouse.RData")   #


# check how many spouse are indexperson or sibling as well for each year
index_delivery <- get(load("index_delivery.RData"))
sib_delivery <- get(load("syskon_info.Rdata"))

index_sib <- unique(c(index_delivery$LopNr, sib_delivery$LopNrSyskon))
length(index_sib)   # 3,751,783

for (year_n in 1998:2017){
	index_spouse_yearn <- index_spouse[index_spouse$year==year_n,]
	print(paste("N. spouse of year ", year_n, " is ", nrow(index_spouse_yearn), ", where ", round(100*sum(index_spouse_yearn$LopNrSamh %in% index_sib)/nrow(index_spouse_yearn),1), "% are indexperson and siblings" ,sep="" ))
}

# [1] "N. spouse of year 1998 is  993486, where 88.8% are indexperson and siblings"
# [1] "N. spouse of year 1999 is 1037423, where 89.0% are indexperson and siblings"
# [1] "N. spouse of year 2000 is 1088166, where 89.2% are indexperson and siblings"
# [1] "N. spouse of year 2001 is 1136325, where 89.4% are indexperson and siblings"
# [1] "N. spouse of year 2002 is 1190180, where 89.5% are indexperson and siblings"
# [1] "N. spouse of year 2003 is 1244226, where 89.6% are indexperson and siblings"
# [1] "N. spouse of year 2004 is 1297548, where 89.6% are indexperson and siblings"
# [1] "N. spouse of year 2005 is 1351080, where 89.7% are indexperson and siblings"
# [1] "N. spouse of year 2006 is 1404941, where 89.6% are indexperson and siblings"
# [1] "N. spouse of year 2007 is 1453336, where 89.5% are indexperson and siblings"
# [1] "N. spouse of year 2008 is 1496491, where 89.4% are indexperson and siblings"
# [1] "N. spouse of year 2009 is 1533323, where 89.2% are indexperson and siblings"
# [1] "N. spouse of year 2010 is 1566887, where 89.0% are indexperson and siblings"
# [1] "N. spouse of year 2011 is 1852883, where 87.1% are indexperson and siblings"
# [1] "N. spouse of year 2012 is 1856890, where 86.8% are indexperson and siblings"
# [1] "N. spouse of year 2013 is 1858215, where 86.6% are indexperson and siblings"
# [1] "N. spouse of year 2014 is 1857799, where 86.3% are indexperson and siblings"
# [1] "N. spouse of year 2015 is 1853925, where 86.2% are indexperson and siblings"
# [1] "N. spouse of year 2016 is 1850132, where 86.0% are indexperson and siblings"
# [1] "N. spouse of year 2017 is 1841224, where 85.8% are indexperson and siblings"


# check whether all spouse (registered between 1998 and 2017) had economic and education level
index_spouse <- get(load("index_spouse.RData"))
#index_sib <- unique(c(index_delivery$LopNr, sib_delivery$LopNrSyskon))
#length(index_sib)   # 3,751,783

for (year_n in 1998:2017){
	rtb_yearn <- get(load(paste("/proj/sens2019018/SCB_data/release3/r_files/tove_lev_rtb_",year_n,".Rdata",sep="")))   
	nrow(rtb_yearn)       # 8,020,303
	
	lisa_yearn <- get(load(paste("/proj/sens2019018/SCB_data/release3/r_files/tove_lev_lisa_",year_n,".Rdata",sep="")))  
	nrow(lisa_yearn)      # 6,656,562
		
	index_spouse_yearn <- index_spouse[index_spouse$year==year_n,]
	
	print(paste("N. of individuals appear in LISA but not indexperson or sibling ",length(setdiff(lisa_yearn$LopNr, index_sib)), sep=""))
	print(paste("N. of individuals appear in RTB but not indexperson or sibling ",length(setdiff(rtb_yearn$LopNr, index_sib)), sep=""))
				
	print(paste("N. of RTB is ", length(rtb_yearn$LopNr),";N. of LISA is ", length(lisa_yearn$LopNr),";N. of overlap between RTB and LISA is ", length(intersect(rtb_yearn$LopNr, lisa_yearn$LopNr))), sep="")
	print(paste(100*length(index_spouse_yearn$LopNr %in% rtb_yearn$LopNr)/nrow(index_spouse_yearn), "perentage spouse are available in RTB"))    # 1 for all analyses
	print(paste(100*length(index_spouse_yearn$LopNr %in% lisa_yearn$LopNr)/nrow(index_spouse_yearn), "perentage spouse are available in LISA"))    # 1 for all analyses
}


# [1] "N. of RTB is  7097904 ;N. of LISA is  5536159 ;N. of overlap between RTB and LISA is  5536159"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7150012 ;N. of LISA is  5598054 ;N. of overlap between RTB and LISA is  5598054"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7205008 ;N. of LISA is  5662376 ;N. of overlap between RTB and LISA is  5662376"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7261780 ;N. of LISA is  5730499 ;N. of overlap between RTB and LISA is  5730499"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7321125 ;N. of LISA is  5799969 ;N. of overlap between RTB and LISA is  5799969"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7380522 ;N. of LISA is  5868965 ;N. of overlap between RTB and LISA is  5868965"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7438307 ;N. of LISA is  5942292 ;N. of overlap between RTB and LISA is  5942292"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7491273 ;N. of LISA is  6014755 ;N. of overlap between RTB and LISA is  6014755"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7546129 ;N. of LISA is  6092496 ;N. of overlap between RTB and LISA is  6092496"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7599465 ;N. of LISA is  6167777 ;N. of overlap between RTB and LISA is  6167777"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7651763 ;N. of LISA is  6238458 ;N. of overlap between RTB and LISA is  6238458"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7708795 ;N. of LISA is  6305808 ;N. of overlap between RTB and LISA is  6305808"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7761813 ;N. of LISA is  6454449 ;N. of overlap between RTB and LISA is  6454449"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7808189 ;N. of LISA is  6493782 ;N. of overlap between RTB and LISA is  6493782"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7853310 ;N. of LISA is  6527191 ;N. of overlap between RTB and LISA is  6527191"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7893755 ;N. of LISA is  6556455 ;N. of overlap between RTB and LISA is  6556455"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7932388 ;N. of LISA is  6583385 ;N. of overlap between RTB and LISA is  6583385"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7964270 ;N. of LISA is  6607363 ;N. of overlap between RTB and LISA is  6607363"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  7995792 ;N. of LISA is  6631674 ;N. of overlap between RTB and LISA is  6631674"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"
# [1] "N. of RTB is  8020303 ;N. of LISA is  6656562 ;N. of overlap between RTB and LISA is  6656562"
# [1] "100 perentage spouse are available in RTB"
# [1] "100 perentage spouse are available in LISA"



# check whether economic and educational info include individuals which are not indexperson or siblings 
sib_delivery <- get(load("syskon_info.Rdata"))
for (year_n in 1998:2017){
	rtb_yearn <- get(load(paste("/proj/sens2019018/SCB_data/release3/r_files/tove_lev_rtb_",year_n,".Rdata",sep="")))   
	nrow(rtb_yearn)       # 8,020,303
	
	lisa_yearn <- get(load(paste("/proj/sens2019018/SCB_data/release3/r_files/tove_lev_lisa_",year_n,".Rdata",sep="")))  
	nrow(lisa_yearn)      # 6,656,562
		

}



index_with_spouse<-unique(index_spouse[,"LopNr"])   # 2,383,045
#background<-get(load("/proj/sens2019018/SCB_data/release1/r_files/tove_lev_bakgrund.Rdata"))   ## nrow=2,893,654 unique id, no duplicate, colnames=c("LopNr","FodelseAr","Kon","FodelseLandNamn","FodelseLandNamnFar","FodelseLandNamnMor","FodelseLan","FodelseKommun","AterPnr")
index<-unique(background[,"LopNr"])        # 2,893,654
index_a<-setdiff(index_with_spouse,index)  # 0, indicates all indexperson with spouse are included in the background file 
length(setdiff(index,index_with_spouse))   # 510,609, indexperson never with partner
spouse_of_index<-unique(index_spouse[,"LopNrSamh"])   # 2,537,765

index_spouse_1<-unique(index_spouse[1:10000000,c("LopNrSamh","FodelseArSamh")])
index_spouse_2<-index_spouse[10000001:20000000,c("LopNrSamh","FodelseArSamh")]
index_spouse_3<-index_spouse[20000001:30000000,c("LopNrSamh","FodelseArSamh")]
index_spouse_4<-index_spouse[30000001:nrow(index_spouse),c("LopNrSamh","FodelseArSamh")]

index_spouse_1<-index_spouse_1[index_spouse_1[,"FodelseArSamh"]>=1956 & index_spouse_1[,"FodelseArSamh"]<=1982,"LopNrSamh"]
index_spouse_2<-index_spouse_2[index_spouse_2[,"FodelseArSamh"]>=1956 & index_spouse_2[,"FodelseArSamh"]<=1982,"LopNrSamh"]
index_spouse_3<-index_spouse_3[index_spouse_3[,"FodelseArSamh"]>=1956 & index_spouse_3[,"FodelseArSamh"]<=1982,"LopNrSamh"]
index_spouse_4<-index_spouse_4[index_spouse_4[,"FodelseArSamh"]>=1956 & index_spouse_4[,"FodelseArSamh"]<=1982,"LopNrSamh"]
index_spouse_19561982<-unique(c(index_spouse_1,index_spouse_2,index_spouse_3,index_spouse_4))    # 2,175,800
spouse_should_be_index<-setdiff(index_spouse_19561982,index)  # 212,851

#check whether there are spouse born within 1956-1982 but not present in background file

 
#---------------------
#1.7.2 spouse of indexperson's sibling (data is too large, divided into two periods)
#for (year_n in 1977:2000){
#for (year_n in 1977:2017){
for (year_n in 2001:2017){
	sib_spouse_yearn<-get(load(paste("/proj/sens2019018/SCB_data/release2/r_files/tove2_lev_syskonpartn",year_n,".Rdata",sep="")))              # LopNrSyskon LopNrSamh FodelseArSamh FodelseLanSamh FodelseKommunSamh KonSamh
	#sib_spouse_yearn<-sib_spouse_yearn[,c("LopNrSyskon","LopNrSamh")]
	sib_spouse_yearn[,"year_n"]<-year_n
	#if (year_n==1977){
	if (year_n==2001){
		sib_spouse<-sib_spouse_yearn
	}else{
		sib_spouse<-rbind(sib_spouse,sib_spouse_yearn)
	}
	print(year_n)
}
#save(sib_spouse,file="sib_spouse_1977_2000.RData")   #
save(sib_spouse,file="sib_spouse_2001_2017.RData")   #


sib_spouse_1977_2000 <- sib_spouse
#save(sib_spouse_1977_2000,file="sib_spouse_1977_2000.RData")   ###
ss_1<-sib_spouse_1977_2000[sib_spouse_1977_2000[,"FodelseArSamh"]>=1956 & sib_spouse_1977_2000[,"FodelseArSamh"]<=1982,]
#ss_1<-unique(sib_spouse_1977_2000[,c("LopNrSamh","FodelseArSamh")])   # 
ss_1_id<-unique(ss_1[,"LopNrSamh"])    # 1,794,320

for (year_n in 2001:2017){
	sib_spouse_yearn<-get(load(paste("/proj/sens2019018/SCB_data/release2/r_files/tove2_lev_syskonpartn",year_n,".Rdata",sep="")))              # LopNrSyskon LopNrSamh FodelseArSamh FodelseLanSamh FodelseKommunSamh KonSamh
	#sib_spouse_yearn<-sib_spouse_yearn[,c("LopNrSyskon","LopNrSamh")]
	sib_spouse_yearn[,"year_n"]<-year_n
	if (year_n==2001){
		sib_spouse<-sib_spouse_yearn
	}else{
		sib_spouse<-rbind(sib_spouse,sib_spouse_yearn)
	}
	print(year_n)
}

sib_spouse_2001_2017<-sib_spouse
#save(sib_spouse_2001_2017,file="sib_spouse_2001_2017.RData")   ###
#ss_2<-unique(sib_spouse_2001_2017[,c("LopNrSamh","FodelseArSamh")])   # 
ss_2<-sib_spouse_2001_2017[sib_spouse_2001_2017[,"FodelseArSamh"]>=1956 & sib_spouse_2001_2017[,"FodelseArSamh"]<=1982,]
ss_2_id<-unique(ss_2[,"LopNrSamh"])    # 1,794,320

ss_id<-unique(c(ss_1_id,ss_2_id))      # 1,937,046
sib_spouse_should_be_index<-setdiff(ss_id,index)   # 188,673 are spouse born between 1956-1982 but not presented in background file
#save(sib_spouse_should_be_index,file="sib_spouse_should_be_index.RData")   ###

length(sib_spouse_should_be_index)  # 188,673
nrow(sib_should_be_index)       # 52,642
length(intersect(sib_should_be_index[,1],sib_spouse_should_be_index))   # 18876
length(setdiff(sib_should_be_index[,1],sib_spouse_should_be_index))     # 14428
length(setdiff(sib_spouse_should_be_index,sib_should_be_index[,1]))     # 169797

sib_with_spouse<-unique(sib_spouse[,"LopNrSyskon"]) # 2,409,801
#syskon<-get(load("/proj/sens2019018/SCB_data/release2/r_files/tove2_lev_koppl_index_syskon.Rdata")) 
sib<-unique(syskon[,"LopNrSyskon"])   # 2,493,352
length(setdiff(sib_with_spouse,sib))  # 0, indicates all siblings with spouse are included in the info file of siblings. No sibling without children presented up here. 
length(setdiff(sib,sib_with_spouse))  # 83551, siblings with children never have partners registered


