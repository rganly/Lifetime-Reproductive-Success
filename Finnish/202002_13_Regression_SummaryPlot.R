
## Summary results from regression analyses  



################################################
##   Format inputs using shell in FIMM ATLAS   #
################################################

cd  /homes/aliu/DSGE_LRS/output/registry_edit

## Sib
head -n 1  pop_reg_sib_selected_endpoint_1_500.tsv
# Endpoint sex Prevalence N.control N.case Avg.NEB.control Avg.NEB.case Variable Estimate Estimate.SE Estimate.Pval Estimate.LowerCI Estimate.UpperCI

less pop_reg_sib_selected_endpoint_*.tsv|grep ENDPOINT|sort|uniq > pop_reg_sib_selected_endpoint.txt
wc -l pop_reg_sib_selected_endpoint.txt  # 3000



## Index person
head -n 1  Population_based_regression_endpoint_specific.tsv
# Endpoint sex Prevalence N.control N.case Avg.NEB.control Avg.NEB.case Variable Estimate Estimate.SE Estimate.Pval Estimate.LowerCI Estimate.UpperCI

cat <(awk 'NR<5383' Population_based_regression_endpoint_1_2339.tsv) \
            <(awk 'NR>90' Population_based_regression_endpoint_1501_2000_qsub.tsv) \
            Population_based_regression_endpoint_{300_500,501_1000,1001_1500,1501_2000,2001_2339}_qsub.tsv|grep ENDPOINT|sort|uniq > pop_reg_results_ALL_FIN.txt

wc -l pop_reg_results_ALL_FIN.txt       # 4688

join -1 1 -2 1 <(awk '{print $1}' pop_reg_sib_selected_endpoint.txt|sort -dk1,1|uniq) <(sort -dk1,1 pop_reg_results_ALL_FIN.txt) > pop_reg_pop_selected_endpoint.txt
wc -l pop_reg_{pop,sib}_selected_endpoint.txt


## copy from ATLAS to local

scp aliu@atlas.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/pop_reg_{pop,sib}_selected_endpoint.txt  /Users/aoxliu/Downloads/
scp aliu@atlas.fimm.fi:/homes/aliu/DSGE_LRS/input/HILMO_UPDATED_FIN.lst  /Users/aoxliu/Downloads/




################################################
##      Plotting results using Rstudio         #
################################################

setwd("/Users/aoxliu/Downloads/")

require(ggplot2)
library(plotly)



## Create function and set the endpoint group

mhd_2 <- function(dat, thres, corlor, pop){
	dat[,"sig"] <- as.character(ifelse(dat$Estimate.Pval<=thres, "Significant", "Not significant"))
	dat[,"al"] <- ifelse(dat$Estimate.Pval<=thres, 0.8, 0.1)
	tit <- ifelse(pop=="pop","Effect of disease on NEB for males (top) and females (bottom) in Finnish population born from 1956 to 1982", "Effect of disease on NEB for unaffected brothers (top) and sisters (bottom) of affected individuals in Finnish population born from 1956 to 1982")
	ylim_min <- ifelse(pop=="pop", -12, -1)
	ylim_max <- 1
	
	pp <- ggplot(data = dat, aes(x = Disease, y = Estimate,
	                            text = paste(sex,"\n", 
	                                         "Endpoint: ", Endpoint, "\n",
	                                         "Long name: ", LONGNAME, "\n",
	                                         "ICD8: ", ICD8, "; ICD9: ", ICD9, "; ICD10: ", ICD10, "\n",
	                                         "Prevalence: ", Prevalence, "\n",
	                                         "N: case: ", N.case, "; Control: ", N.control, "\n",    
	                                         "Avg(NEB): Case: ", Avg.NEB.case, "; Control: ", Avg.NEB.control, "\n",  
	                                         "Estimate: ", Estimate, "\n",
	                                         "Pval: ", Estimate.Pval, "\n", sep = ""))) +
	      # geom_point(aes(shape=Variable, color=sex),alpha=1)
	      # geom_point(alpha=1, shape=1) + 
	      geom_point(aes(color=Population, alpha=al), shape=1, size=0.6) + 
	      scale_shape_manual(values=c(1, 3))+
	     #  scale_color_manual(values=c('VIOLETRED4','steelblue4'))+
	     # scale_color_manual(values=c('VIOLETRED4'))+
	     scale_color_manual(values=c(corlor))+
	      #labs(x = "FinnGen samples (born 1956-1982)", y = "Index person (born 1956-1982)", title="Prevalence (%) of endpoint") +
	      labs(y="Effect of disease") +
	      geom_hline(yintercept = 0, linetype="dotted", color = "grey", size=0.3) +
	      geom_vline(xintercept = hlines, linetype="dotted", color = "grey", size=0.3) + 
	      xlim(0, 2500) + 
	      ylim(ylim_min, ylim_max) + 
	     # xlim(0, 2500) + ylim(-5, 1) + 
	      scale_x_continuous(breaks=xlabs, label=xlabs_text) +
	      theme_classic() +
	      labs(title=tit, face="bold")
	      
    pp <- pp + theme(axis.text.x = element_text(size=8, angle=45), plot.title = element_text(size=12, face="bold"))
    return(pp)
}



huf <- read.table("HILMO_UPDATED_FIN.lst", sep="\t", header=T)    # endpoints and corresponding icd8-10
head(huf)

ep_grp <- matrix(c("ALCOPANCACU",       "K11",
                   "CARMYOPATHALCO",    "I9",
                   "EPISEIZALCO",       "G6",
                   "GEST",              "O15",
                   "HEPATITIS",         "AB1",
                   "PNEUMO",            "AB1",
                   "RESP",              "J10",
                   "RHEUMA",            "M13",
                   "TRAUMBRAIN",        "OTHER",
                   "TUBERCULOSIS",      "AB1", 
                   "ALCOGASTRITIS",     "K11",  
                   "ALCOHOLMENTAL",     "F5", 
                   "ALCOLIVER",         "K11",
                   "ALCONEURODEGEN",    "G6",
                   "ALCOPANCCHRON",     "K11",
                   "ALCOPOLYNEU",       "G6",
                   "CHARCOT",           "OTHER",
                   "CHILDHOOD",         "OTHER",
                   "CHIRHEP",           "K11",
                   "COPD",              "J10",
                   "DENTAL",            "OTHER",
                   "DRY",               "H8",
                   "FTD",               "OTHER",
                   "GIANT",             "OTHER",
                   "ILD",               "J10",
                   "INFLUENZA",         "J10",
                   "LUNG",              "CD3",
                   "MACULA",            "H8",
                   "PANCREATITIS",      "K11",
                   "PDSTRICT",          "G6",
                   "PNEUMOBACTKNOWN",   "J10",
                   "PSOR",              "M13",
                   "SPONDYLOARTHRITIS", "M13",
                   "ST19",              "OTHER",
                   "T2D",               "E4",
                   "THYROIDITIS",       "E4",
                   "WET",               "H8",
                   "SCND",              "M13",
                   "ASTHMA",            "J10",
                   "MACULAR",           "H8",
                   "O15",               "O15",
                   "KRA",               "F5",
                   "AB1",               "AB1",
                   "DM",                "E4",
                   "J10",               "J10",
                   "N14",               "N14",
                   "C3",                "CD2",
                   "D3",                "D3",
                   "H8",                "H8",
                   "I9",                "I9",
                   "K11",               "K11",
                   "F5",                "F5",
                   "G6",                "G6",
                   "CD2",               "CD2",
                   "E4",                "E4",
                   "L12",               "L12",
                   "H7",                "H7",
                   "M13",               "M13"), byrow=T, ncol=2)
colnames(ep_grp) <- c("group","grp_icd10")
	
ep_grp_text <- matrix(c("OTHER", "Other",
                   "O15",   "Pregnancy, childbirth & puerperium",
                   "AB1",   "Infectious & parasitic",
                   "J10",   "Respiratory system",
                   "N14",   "Genitourinary system",
                   "D3",    "Blood & immune mechanism",
                   "H8",    "Ear & mastoid process",
                   "I9",    "Circulatory system",
                   "K11",   "Digestive system",
                   "F5",    "Mental & behavioural",
                   "G6",    "Nervous system",
                   "CD2",   "Neoplasms",
                   "E4",    "Endocrine, nutritional & metabolic",
                   "L12",   "Skin & subcutaneous tissue",
                   "H7",    "Eye & adnexa",
                   "M13",   "Musculoskeletal system"), byrow=T, ncol=2)
colnames(ep_grp_text) <- c("grp_icd10","grp_text")
	
ep <- merge(ep_grp, ep_grp_text, by="grp_icd10")
ep[] <- lapply(ep, as.character)





#-------------------------
## Start plotting


Population <- "FIN"
thres <- 0.05/1500



for (pop_n in c("pop","sib")){
	res <- read.table(paste0("pop_reg_", pop_n,"_selected_endpoint.txt"), header=F)	
	colnames(res) <- c("Endpoint","sex","Prevalence","N.control","N.case","Avg.NEB.control","Avg.NEB.case","Variable","Estimate","Estimate.SE","Estimate.Pval","Estimate.LowerCI","Estimate.UpperCI")

	res <- merge(res, huf, by.x="Endpoint", "ENDPOINT")
	dim(res)
	
	res$group <- sub("_.*", "", as.character(res$Endpoint))
	
	res <- merge(res, ep, by="group")
	res <- res[res$grp_icd10!="OTHER",]
	res[,"Population"] <- substr(res$Variable, 10, 20)
	res[,"LogP"] <- -log10(res$Estimate.Pval)
	res <- res[order(res$grp_icd10, res$ICD10, res$Endpoint),]
	res$Disease <- rep(seq(1,nrow(res)/2,1),each=2)
	res <- res[res$Prevalence>0.01,]
	hlines <- aggregate(Disease ~ grp_text, res, max)[,"Disease"] + 0.5
	xlabs <- 0.5 * aggregate(Disease ~ grp_text, res, min)[,"Disease"] + 0.5 * aggregate(Disease ~ grp_text, res, max)[,"Disease"]
	xlabs_text <- aggregate(Disease ~ grp_text, res, min)[,"grp_text"]

	
	res_mm <- ggplotly(mhd_2(res[res$sex=="male", ], thres, 'steelblue4', pop_n) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + theme(legend.position = "none"), tooltip="text")
	res_ff <- ggplotly(mhd_2(res[res$sex=="female", ], thres, 'VIOLETRED4', pop_n), tooltip="text")
	subplot(res_mm, res_ff, nrows=2) %>% layout(showlegend = FALSE)

}	




	

