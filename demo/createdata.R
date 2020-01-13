library(synthpop)
library(BigSyn)
library(foreach)
papath<-file.path(Mydirectories::googledrive.directory(),"Templates_and_tools/Paquets_R/BigSyn")
temp.dir<-tempdir()
#sapply(list.files(file.path(papath,"R")),function(x){source(file.path(papath,"R",x))})
tableA<-sampledata()
XTKA<-Tsampledata()
TtableA<-XTKA$TtableA
XKA<-XTKA[c("key","variables")]
TTtableA<-BigSyn::GeneralReversetransposefunction(TtableA = TtableA,key = XTKA$key)
STtableA<-SDPSYN2(TtableA,fitmodelsavepath = temp.dir,asis=XKA$key$id1,nrep=3,synparameters = list(ntree=4))
SynreportA <-STtableA$Sparameters
STtableA<-STtableA$STtableA
names(STtableA)<-paste0("STtableA",1:length(STtableA))
attach(STtableA)
TSTtableA<-lapply(STtableA,GeneralReversetransposefunction,XTKA$key)
names(TSTtableA)<-paste0("TSTtableA",1:length(STtableA))
attach(TSTtableA)

save(tableA,file=file.path(papath,"data/tableA.rda"))
save(TtableA,file=file.path(papath,"data/TtableA.rda"))
save(TTtableA,file=file.path(papath,"data/TTtableA.rda"))
save(XKA,file=file.path(papath,"data/XKA.rda"))
sapply(1:length(STtableA),function(i){eval(parse(text=paste0('save(STtableA',i,',file=file.path(papath,"data/STtableA',i,'.rda"))')))})
save(SynreportA,file=file.path(papath,"data/SynreportA.rda"))
sapply(1:length(STtableA),function(i){eval(parse(text=paste0('save(TSTtableA',i,',file=file.path(papath,"data/TSTtableA',i,'.rda"))')))})

tableB<-sampledata(FALSE)
XTKB<-Tsampledata(FALSE)
TtableB<-XTKB$TtableA
XKB<-XTKB[c("key","variables")]
TTtableB<-GeneralReversetransposefunction(XTKB$TtableA,XTKB$key)
STtableB<-SDPSYN2(TtableB,asis=XKB$key$id1,fitmodelsavepath = temp.dir,nrep=3)
SynreportB <-STtableB$Sparameters
STtableB<-STtableB$STtableA
names(STtableB)<-paste0("STtableB",1:length(STtableA))
attach(STtableB)
TSTtableB<-lapply(STtableB,GeneralReversetransposefunction,XTKB$key)
names(TSTtableB)<-paste0("TSTtableB",1:length(STtableA))
attach(TSTtableB)

save(tableB,file=file.path(papath,"data/tableB.rda"))
save(TtableB,file=file.path(papath,"data/TtableB.rda"))
save(TTtableB,file=file.path(papath,"data/TTtableB.rda"))
save(XKB,file=file.path(papath,"data/XKB.rda"))
sapply(1:length(STtableB),function(i){eval(parse(text=paste0('save(STtableB',i,',file=file.path(papath,"data/STtableB',i,'.rda"))')))})
save(SynreportB,file=file.path(papath,"data/SynreportB.rda"))
sapply(1:length(STtableB),function(i){eval(parse(text=paste0('save(TSTtableB',i,',file=file.path(papath,"data/TSTtableB',i,'.rda"))')))})

rm(list=ls())
