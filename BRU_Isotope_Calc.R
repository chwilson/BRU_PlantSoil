#### Various workhorse functions 

#converts from delta into atom % 
atom_Cent <- function(delta_val) {
  AR <- 0.0111803
  atom_cent <- (100*AR*(delta_val/1000 + 1))/(1 + AR*(delta_val/1000 + 1))
  return(atom_cent/100)
}


atom_Cent(200)
atom_Cent(-15)
atom_Cent(2000)


atom_Cent_N <- function(delta_val) {
  AR <- 0.3663
  atom_cent <- (100*AR*(delta_val/1000 + 1))/(1 + AR*(delta_val/1000 + 1))
  return(atom_cent/100)
}


atom_Cent_d <- function(delta_val) {
  AR <- 1/6420
  atom_cent <- (100*AR*(delta_val/1000 + 1))/(1 + AR*(delta_val/1000 + 1))
  return(atom_cent/100)
}

atom_Cent_N(197)


##### Computing mass of label added 
colnames(AG_datasheet)[5:8] <- c("d15N","d13C","centN","centC")
AG_plantDat <- AG_datasheet[5:20,]
AG_plantDat$Spp <- c(rep(paste("RP"),8),rep(paste("BG"),8))
library(dplyr)

Iso_tracer_added <- AG_plantDat %>%  mutate(added_13C = 0.01*3*centC*(atom_Cent(d13C)-atom_Cent(-18)),
                                                              added_15N = 0.01*3*centC*(atom_Cent(d15N)-atom_Cent(2))) %>%
  group_by(Spp) %>% summarize(mean13Cadd = mean(added_13C), SE13Cadd = sd(added_13C)/sqrt(8), mean15Nadd = mean(added_15N),
                              SE15Nadd = sd(added_15N)/sqrt(8))







