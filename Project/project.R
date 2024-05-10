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


write.csv(ch.full, here("Project", "salch.csv")) #write out CSV to take over to RMark on a PC
#code for RMark script from PC is pasted below
salch <- read.csv("salch.csv", colClasses=c("ch"="character"))

salch.processed <- process.data(salch,model="CJS",begin.time=1,groups=c("sex", "stage", "tail", "stream", "trout")) #"processed" data object

Phi.dot <- list(formula=~1) #constant Phi
p.dot   <- list(formula=~1) #constant p
Phi.t <- list(formula=~time) 
p.t   <- list(formula=~time)
Phi.sex <- list(formula=~sex) 
Phi.stage <- list(formula=~stage) 
Phi.tail <- list(formula=~tail) 
Phi.stream <- list(formula=~stream) 
p.stream  <- list(formula=~stream)
Phi.trout <- list(formula=~trout) 
Phi.all <- list(formula=~time+sex+stage+tail+stream+trout)
p.all <- list(formula=~time+stream)
Phi.nosex <- list(formula=~time+stage+tail+stream+trout)
Phi.nosextail <- list(formula=~time+stage+stream+trout)
Phi.nosextailtrout <- list(formula=~time+stage+stream)
Phi.timestream <- list(formula=~time+stream)

#hypothesis for phi: null, time, sex, stage, tail, stream, trout
#hypoethesis for p: null, time, stream

Phi.dot.p.dot <- mark(salch.processed,model.parameters=list(Phi=Phi.dot,p=p.dot)) #null model
Phi.dot.p.time <- mark(salch.processed,model.parameters=list(Phi=Phi.dot,p=p.t))
Phi.dot.p.stream<- mark(salch.processed,model.parameters=list(Phi=Phi.dot,p=p.stream))
Phi.dot.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.dot,p=p.all))
Phi.t.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.t,p=p.all))
Phi.sex.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.sex,p=p.all))
Phi.stage.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.stage,p=p.all))
Phi.tail.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.tail,p=p.all))
Phi.stream.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.stream,p=p.all))
Phi.trout.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.trout,p=p.all))
Phi.all.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.all,p=p.all)) #full model
Phi.nosex.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.nosex,p=p.all))
Phi.nosextail.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.nosextail,p=p.all))
Phi.nosextailtrout.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.nosextailtrout,p=p.all))
Phi.timestream.p.all<- mark(salch.processed,model.parameters=list(Phi=Phi.timestream,p=p.all))

collect.models()