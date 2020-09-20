## download files from bianca
#  cp /home/aoxing/lrs/output/registry_edit/endpoint_f.tsv  /proj/sens2019018/nobackup/wharf/aoxing/aoxing-sens2019018   # from bianca termianl copy files from bianca to wharf, since only wharf has internet connection due to the security reason
#  ssh -A aoxing@rackham.uppmax.uu.se                                            # login rackham in another window
#  sftp -q aoxing-sens2019018@bianca-sftp.uppmax.uu.se:aoxing-sens2019018        # login the sftp prompt of bianca
#  get endpoint_f.tsv                                                            # download in the sftp prompt
#  scp aoxing@rackham.uppmax.uu.se:/domus/h1/aoxing/endpoint_f.tsv        /Users/aoxliu/Documents/LRS/Data/SWE/endpoint_SWE_f.tsv   # using mac terminal 


# scp aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/endpointW_f_COMPLETE.lst  /Users/aoxliu/Documents/LRS/Data/FIN
library(htmltools)


ep <- read.table("/Users/aoxliu/Documents/LRS/Data/FIN_SWE/Result/ENDPOINT_PREVALENCE", sep="\t", header=T)
colnames(ep)[2:4] <- c("Prevalence_FIN_index","Prevalence_SWE_index","Prevalence_FINNGEN_index")

ep_f_FIN <- read.table("/Users/aoxliu/Documents/LRS/Data/FIN/endpointW_f_COMPLETE.lst", sep="\t", header=T)
ep_f_FIN$Prevalence_FIN_indexWN <- as.numeric(as.character(ep_f_FIN$COUNT_INDEX_CASES))/1810220

ep_W <- merge(ep, ep_f_FIN[,c("ENDPOINT",paste0("ICD",8:10), "morpho","Prevalence_FIN_indexWN")], by="ENDPOINT")


# ep_W$Prevalence_FINNGEN_index <- round(100*ep_W$Prevalence_FINNGEN_index,2)
# ep_W$Prevalence_SWE_index <- round(100*ep_W$Prevalence_SWE_index,2)
# ep_W$Prevalence_FIN_index <- round(100*ep_W$Prevalence_FIN_index,2)
# ep_W$Prevalence_FIN_indexWN <- round(100*ep_W$Prevalence_FIN_indexWN,2)



pp <- ggplot(data = ep, aes(x = Prevalence_FINNGEN_index, y = Prevalence_SWE_index,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in FinnGen samples (born 1956-1982) : ", Prevalence_FINNGEN_index, " %", "\n",
                                         "Prevalence in Swedish index person (born 1956-1982): ", Prevalence_SWE_index, " %", "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (%) in FinnGen samples (born 1956-1982)", y = "Prevalence (%) in Swedish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(0, 40) + ylim(0, 40) + 
      theme_classic()

ggplotly(pp, tooltip = "text")

save_html(ggplotly(pp, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/Prevalence_FinnGen_SWE.html")




ep_W$log_Prevalence_FINNGEN_index <- log10(as.numeric(as.character(ep_W$Prevalence_FINNGEN_index)))
ep_W$log_Prevalence_SWE_index <- log10(as.numeric(as.character(ep_W$Prevalence_SWE_index)))
ep_W$log_Prevalence_FIN_index <- log10(as.numeric(as.character(ep_W$Prevalence_FIN_index)))
ep_W$log_Prevalence_FIN_indexWN <- log10(as.numeric(as.character(ep_W$Prevalence_FIN_indexWN)))


pp <- ggplot(data = ep, aes(x = log_Prevalence_FINNGEN_index, y = log_Prevalence_SWE_index,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "FinnGen samples (born 1956-1982) : ", Prevalence_FINNGEN_index, " %", "\n",
                                         "Swedish index person (born 1956-1982): ", Prevalence_SWE_index, " %", "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (log10) in FinnGen samples (born 1956-1982)", y = "Prevalence (log10) in Swedish index person (born 1956-1982)", title="Prevalence of endpoint") +
      geom_abline(intercept = 0, slope = 1, , color = "#51A0D5", size=0.5, linetype="dashed", size=0.5) +
#      xlim(0, 40) + ylim(0, 40) + 
      theme_classic()

ggplotly(pp, tooltip = "text")

save_html(ggplotly(pp, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/Prevalence_FinnGen_SWE.html")



pp <- ggplot(data = ep, aes(x = log_Prevalence_SWE_index, y = log_Prevalence_FIN_index,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Swedish index person (born 1956-1982) : ", log_Prevalence_SWE_index, " %", "\n",
                                         "Finnish index person (born 1956-1982): ", log_Prevalence_FIN_index, " %", "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (log10) in Swedish index person (born 1956-1982)", y = "Prevalence (log10) in Finnish index person (born 1956-1982)", title="Prevalence of endpoint") +
      geom_abline(intercept = 0, slope = 1, , color = "#51A0D5", size=0.5, linetype="dashed", size=0.5) +
#      xlim(0, 40) + ylim(0, 40) + 
      theme_classic()

ggplotly(pp, tooltip = "text")




###########
###########
###########

qq <- ggplot(data = ep_W, aes(x = Prevalence_FINNGEN_index, y = Prevalence_FIN_indexWN,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in FinnGen samples (born 1956-1982) : ", Prevalence_FINNGEN_index, " %", "\n",
                                         "Prevalence in Finnish index person (born 1956-1982): ", Prevalence_FIN_indexWN, " %", "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (%) in FinnGen samples (born 1956-1982)", y = "Prevalence (%) in Finnish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(0, 40) + ylim(0, 40) + 
      theme_classic()

ggplotly(qq, tooltip = "text")

save_html(ggplotly(qq, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/Prevalence_FinnGen_FINWN.html")


qq <- ggplot(data = ep_W, aes(x = Prevalence_FINNGEN_index, y = Prevalence_SWE_index,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in FinnGen samples (born 1956-1982) : ", Prevalence_FINNGEN_index, " %", "\n",
                                         "Prevalence in Swedish index person (born 1956-1982): ", Prevalence_SWE_index, " %", "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (%) in FinnGen samples (born 1956-1982)", y = "Prevalence (%) in Finnish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(0, 40) + ylim(0, 40) + 
      theme_classic()

ggplotly(qq, tooltip = "text")

save_html(ggplotly(qq, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/Prevalence_FinnGen_SWE.html")



qq <- ggplot(data = ep_W, aes(x = Prevalence_FIN_indexWN, y = Prevalence_SWE_index,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in Finnish index person samples (born 1956-1982) : ", Prevalence_FIN_indexWN, " %", "\n",
                                         "Prevalence in Swedish index person (born 1956-1982): ", Prevalence_SWE_index, " %", "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (%) in FinnGen samples (born 1956-1982)", y = "Prevalence (%) in Finnish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(0, 40) + ylim(0, 40) + 
      theme_classic()

ggplotly(qq, tooltip = "text")

save_html(ggplotly(qq, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/Prevalence_FINWN_SWE.html")

######
log_qq <- ggplot(data = ep_W, aes(x = log_Prevalence_SWE_index, y = log_Prevalence_FIN_indexWN, 
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in Finnish index person (born 1956-1982): ", Prevalence_FIN_indexWN, "\n",
                                         "Prevalence in Swedish index person (born 1956-1982): ", Prevalence_SWE_index, "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (log10) in Swedish index person  (born 1956-1982)", y = "Prevalence (log10) in Finnish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(-6, -1) + ylim(-6, -1) + 
      theme_classic()

ggplotly(log_qq, tooltip = "text")

save_html(ggplotly(log_qq, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/FIN_SWE/Plot/NEW_Prevalence_LOG_SWE_FINWN.html")


log_oo <- ggplot(data = ep_W, aes(x = log_Prevalence_SWE_index, y = log_Prevalence_FIN_index,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in Finnish index person (born 1956-1982): ", Prevalence_FIN_index, "\n",
                                         "Prevalence in Swedish index person (born 1956-1982): ", Prevalence_SWE_index, "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (log10) in Swedish index person  (born 1956-1982)", y = "Prevalence (log10) in Finnish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(-6, -1) + ylim(-6, -1) + 
      theme_classic()

ggplotly(log_oo, tooltip = "text")

save_html(ggplotly(log_oo, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/FIN_SWE/Plot/NEW_Prevalence_LOG_SWE_FINOO.html")




#---------------------

log_qq <- ggplot(data = ep_W, aes(x = log_Prevalence_FINNGEN_index, y = log_Prevalence_FIN_indexWN, 
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in Finnish index person (born 1956-1982): ", Prevalence_FIN_indexWN, "\n",
                                         "Prevalence in FinnGen samples (born 1956-1982): ", Prevalence_FINNGEN_index, "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (log10) in FinnGen samples (born 1956-1982)", y = "Prevalence (log10) in Finnish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(-6, -1) + ylim(-6, -1) + 
      theme_classic()

ggplotly(log_qq, tooltip = "text")

save_html(ggplotly(log_qq, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/FIN_SWE/Plot/NEW_Prevalence_LOG_FINNGEN_FINWN.html")


log_oo <- ggplot(data = ep_W, aes(x = log_Prevalence_FINNGEN_index, y = log_Prevalence_FIN_index,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "Prevalence in Finnish index person (born 1956-1982): ", Prevalence_FIN_index, "\n",
                                         "Prevalence in FinnGen samples (born 1956-1982): ", Prevalence_FINNGEN_index, "\n", sep = ""))) +
      geom_point(alpha=0.7, colour = "#51A0D5") + 
      labs(x = "Prevalence (log10) in FinnGen samples (born 1956-1982)", y = "Prevalence (log10) in Finnish index person (born 1956-1982)") +
      geom_abline(intercept = 0, slope = 1, , color = "#2C528C", size=0.5) +
      xlim(-6, -1) + ylim(-6, -1) + 
      theme_classic()

ggplotly(log_oo, tooltip = "text")

save_html(ggplotly(log_oo, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/FIN_SWE/Plot/NEW_Prevalence_LOG_FINNGEN_FINOO.html")





################


dat_f <- ep[,c("ENDPOINT","LONGNAME","Prevalence_FINNGEN_index","Prevalence_FIN_index")]
dat_f[,"Country"] <- "Finnish"
dat_s <- ep[,c("ENDPOINT","LONGNAME","Prevalence_FINNGEN_index","Prevalence_SWE_index")]
dat_s[,"Country"] <- "Swedish"

colnames(dat_f) <- colnames(dat_s) <- c("ENDPOINT","LONGNAME","Prevalence_FINNGEN","Prevalence_index","Country")
dat <- rbind(dat_f, dat_s)

aa <- ggplot(data = dat, aes(x = Prevalence_FINNGEN, y = Prevalence_index, color = Country,
                            text = paste("Endpoint: ", ENDPOINT, "\n",
                                         "Long name: ", LONGNAME, "\n",
                                         "FinnGen samples: ", Prevalence_FINNGEN, " %", "\n",
                                         Country," index person: ", Prevalence_index, " %", "\n", sep = ""))) +
     # geom_point(alpha=0.7) + 
      geom_point(alpha=1, shape=1) + 
      labs(x = "FinnGen samples (born 1956-1982)", y = "Index person (born 1956-1982)", title="Prevalence (%) of endpoint") +
      geom_abline(intercept = 0, slope = 1, color = "#51A0D5", linetype="dashed", size=0.5) +
      xlim(0, 0.4) + ylim(0, 0.4) + 
      theme_classic()

ggplotly(aa, tooltip = "text")

save_html(ggplotly(qq, tooltip = "text"), "/Users/aoxliu/Documents/LRS/Data/Prevalence_FinnGen_FIN.html")
