## This script is to plot prevalence of endpoints from two datasets

setwd("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Data/FIN_SWE/Plot/")

library(ggplot2)
library(plotly)
library(htmltools)



##########################
#       Function         #
##########################

plot_preval <- function(dat, x_var, y_var, x_name, y_name, extra_name){
	p <- ggplot(data=dat, aes(x=log10(as.numeric(as.character(dat[,x_var]))), y=log10(as.numeric(as.character(dat[,y_var]))),
				text=paste0("Endpoint: ", ENDPOINT, "\n",
				"Long name: ", LONGNAME, "\n",
				"ICD8: ", ICD8, "\n",
				"ICD9: ", ICD9, "\n",
				"ICD10: ", ICD10, "\n",
				paste0("Prevalence in ", x_name, ": "), dat[,x_var], "\n",
				paste0("Prevalence in ", y_name, ": "), dat[,y_var], "\n"))) +
				geom_point(alpha=0.7, colour = "#51A0D5") + 
				labs(y=paste0("Prevalence (log scale) in ", y_name), x=paste0("Prevalence (log scale) in ", x_name)) +
				geom_abline(intercept = 0, slope=1, , color="#2C528C", size=0.5) +
				xlim(-7, 0) + ylim(-7, 0) + 
				theme_classic()
	save_html(ggplotly(p, tooltip="text"), paste0("Prevalence_",x_var,"_",y_var,"_",extra_name,".html"))
}



##########################
#       Data             #
##########################

ep_f_FIN <- read.table("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Data/FIN/endpointW_f_COMPLETE.lst", sep="\t", header=T)
ep_f_FIN$Prevalence_FIN_indexWN <- as.numeric(as.character(ep_f_FIN$COUNT_INDEX_CASES))/1810220
nrow(ep_f_FIN)

ep_f_SWE <- read.table("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Data/SWE/SWE_ENDPOINT_sort.tsv", sep="\t", header=T)
nrow(ep_f_SWE)

ep_W <- merge(ep_f_SWE, ep_f_FIN[,c("ENDPOINT","COUNT_INDEX_CASES","Prevalence_FIN_indexWN")], by="ENDPOINT")

ep_W_f50 <- ep_W[ep_W$COUNT_INDEX_CASES>=50|ep_W$count_IndexW>=50,]
nrow(ep_W_f50)   # 1,659

ep_W_b50 <- ep_W[(ep_W$COUNT_INDEX_CASES>=50) & (ep_W$count_IndexW>=50),]
nrow(ep_W_b50)   # 1,438



##########################
#       Plot             #
##########################

plot_preval(dat=ep_W, x_var="Prevalence_IndexW", y_var="Prevalence_FIN_indexWN", x_name="Finnish index person (born 1956-1982)", y_name="Swedish index person (born 1956-1982)", extra_name="EndpointsAll")
plot_preval(dat=ep_W_f50, x_var="Prevalence_IndexW", y_var="Prevalence_FIN_indexWN", x_name="Finnish index person (born 1956-1982)", y_name="Swedish index person (born 1956-1982)", extra_name="Endpoints50Count_either")
plot_preval(dat=ep_W_b50, x_var="Prevalence_IndexW", y_var="Prevalence_FIN_indexWN", x_name="Finnish index person (born 1956-1982)", y_name="Swedish index person (born 1956-1982)", extra_name="Endpoints50Count_both")





