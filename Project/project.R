library(here)
library(tidyverse)
library(RMark)

rawdata <- read.csv(here("Project", "knb-lter-hbr","HubbardBrook_SalamanderCaptureRecapture.csv"))
sals <- rawdata
sals <- sals %>% mutate(ID = coalesce(ElastomerID, PITTagID))

#deal with troublesome NAs, assign survey number by date
sals[5677, 4]=1
sals[5678, 4]=1
sals[5679, 4]=1

#create capture histories
ch <- as.data.frame(matrix(0, ncol = 73, nrow=length(unique(sals$ID))))
colnames(ch) <- c("ID", 1:72)
ch$ID <- unique(sals$ID)                    
for(i in 1:length(sals$ID)){
  id.temp <- sals$ID[i]
  sur.temp <- sals$SurNum[i]
  ch[ch$ID==id.temp, sur.temp+1] <- 1
}
ch.full=data.frame(ch %>% unite(col=ch, 2:73, sep=""))

#add covariates of interest to capture histories                   
ch.full$sex <- 0
ch.full$stage <- 0
ch.full$tail <- 0
ch.full$stream <- 0
ch.full$trout <- 0
for(i in 1:length(sals$ID)){
  ch.full$sex[ch$ID==sals$ID[i]] <- sals$Sex[i] 
  ch.full$stage[ch$ID==sals$ID[i]] <- sals$Stage[i]
  if(is.na(sals$TailRemoved[i]) | sals$TailRemoved[i]>0){  #reclassify tail missing as a categorical variable
  ch.full$tail[ch$ID==sals$ID[i]] <- "Full"
  }
  else{
  ch.full$tail[ch$ID==sals$ID[i]] <- "Damaged"
  }
  ch.full$stream[ch$ID==sals$ID[i]] <- sals$Stream[i]
  if(sals$Reach[i]=="upper"){  #reclassify "reach" as "trout presence" to better reflect the question
    ch.full$trout[ch$ID==sals$ID[i]] <- "Absent"
  }
  else{
    ch.full$trout[ch$ID==sals$ID[i]] <- "Present"
  }
  }
#handle NAs for Sex -- reassign all NAs to U
ch.full$sex <- ch.full$sex %>% replace_na("U")


