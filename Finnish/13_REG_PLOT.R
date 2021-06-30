
### Aim: (Interactive) Plot for effects of all disease endpoints for fertility phenotype in a specific sex


####################################################
#       create function and set environments       #
####################################################


setwd("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Survival_analysis/")   # change to the directory with input data
library(dplyr)
require(ggplot2)
library(plotly)
library(ggrepel)
library(htmlwidgets)


sexs <- c("male","female")
# res_ep_info <- read.table("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Rshiny/IndependentEndpoints_Info.tsv", sep="\t", header=T)
res_ep_info <- read.table("/homes/aliu/DSGE_LRS/input/IndependentEndpoints_Info.tsv", sep="\t", header=T)


mhd_index <- function(dat, NEB, sex, model, variable){
	corlor <- ifelse(sex=="male",'steelblue4','VIOLETRED4')
	#dat[,"corlor"] <- ifelse(dat$HR<1,'steelblue4','VIOLETRED4')
	dat$al <- ifelse(dat$P_val<=0.05/nrow(dat), 0.8, 0.02)
	dat <- dat[order(dat$grp_icd10, dat$ICD10, dat$Endpoint),]
	dat$Disease <- seq(1,nrow(dat),1)

	hlines <- aggregate(Disease ~ grp_text, dat, max)[,"Disease"] + 0.5
	xlabs <- 0.5 * aggregate(Disease ~ grp_text, dat, min)[,"Disease"] + 0.5 * aggregate(Disease ~ grp_text, dat, max)[,"Disease"]
	xlabs_text <- aggregate(Disease ~ grp_text, dat, min)[,"grp_text"]

	tit <- paste0("Impact of disease on ",NEB," in ",ifelse(sex=="male","men","women")," using ",model, " model")
	
	pp <- ggplot(data=dat, aes(x=Disease, y=dat[,variable], label=LONGNAME, 
	                text=paste(sex,"\n", "Endpoint: ", Endpoint, "\n", "Long name: ", LONGNAME, "\n",
	                         "ICD8: ", ICD8, "; ICD9: ", ICD9, "; ICD10: ", ICD10, "\n",
	                         "Estimate: ", Estimate, "\n",
	                         "SE: ", SE, "\n",
	                         "P-value: ", P_val, "\n",
	                         "HR.meta: ", HR, " (", HR_025 ,"-", HR_975,")", "\n",
	                          sep = ""))) + 
	     geom_point(aes(color="", alpha=al), size=0.8) + 
	     # geom_point(aes(color="", alpha=al), shape=1, size=0.8) + 
	     # scale_shape_manual(values=c(1, 3))+
	     scale_color_manual(values=c(corlor))+
	     labs(y="log(HR)") +
	     geom_hline(yintercept = 0, linetype="dotted", color = "grey", size=0.3) +
	     geom_vline(xintercept = hlines, linetype="dotted", color = "grey", size=0.3) + 
	   # ylim(0, 1.5) + 
	     scale_x_continuous(breaks=xlabs, label=xlabs_text) +
	     theme_classic() +
	     labs(title=tit, size=13, face="bold") + theme(legend.position="none") 
	
    pp <- pp + theme(axis.text.x = element_text(size=10, angle=45, hjust = 1, face = "bold"), plot.title = element_text(size=12, face="bold"))
    # pp <- pp + geom_text_repel(data= subset(dat, P_val<0.005/nrow(dat)), size = 2,
    #              box.padding  = 0.5, point.padding = 0.5, force = 100, segment.size  = 0.18, segment.color = "grey50")
    return(pp)
}





######################################
#           Start to plot            #
######################################

# cd  /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Summary_plots/survival
# scp aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/coxph_*_*male_done.tsv   .
# scp aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/outcome_window_*_*male_case50.tsv   .
# scp aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/logit_*_childless_*male_30.tsv   .

sex_n <- 1
# outcomeName <- "childless"
# outcomeName <- "parity"  
# outcomeName <- "NofChildren"  
# outcomeName <- "spouseless"
# outcomeName <- "AgeFirstBirth"


for (sex_n in 1:2) {

	## childless ------------
	outcomeName <- "childless"
	mod_pattern <- "sibmatch"
	
	res <- read.table(paste0("cox_vary_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_50.tsv"), header=T)
	res <- res %>% filter(!is.na(Estimate) & N_00>5 & N_11>5 & N_01>5 & N_10>5)
	# colnames(res) <- c("Estimate", "HR", "SE", "Z",	"P_val", "HR_025", "HR_975", "N_Family", "N_Sample", "N_Record", "model", "sex", "LRS", "Endpoint")
	nrow(res)  # 1,397
	res <- res %>% inner_join(res_ep_info, by="Endpoint") 
	nrow(res)  # 1,397
	p_raw <- mhd_index(dat=res, NEB="has a child", sex=sexs[sex_n], model=paste0("cox_vary_",mod_pattern," model consider disease status as time-varying covariate"), variable="Estimate")
	ggsave(paste0("cox_vary_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_50.png"), width=13, height=5)
	ply <- ggplotly(p_raw + theme(legend.position = "none"), tooltip="text")
	saveWidget(ply, file=paste0("cox_vary_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_50.html"))
	
}

