#treedepth<-function(x){if(x$terminal){1}else{1+max(treedepth(x$right),treedepth(x$left))}}

#' Ctree to pdf graph
#' @param partykitctree: an output of partykit::ctree
#' @param savepath: a file path where to store the pdf of the plot
#' @example
#' y=iris$Species;x=iris[,-5]
#' partykitctree <- partykit::ctree(y ~ ., data=cbind(y=y,x))

treetopdf<-function(partykitctree,savepath){
  depthoftree<-9.6;
  widthoftree<-9.6;
  try(widthoftree<-width(partykitctree));
  try(depthoftree<-depth(partykitctree));
  pdf(savepath,width=4+3*widthoftree,height=4+max(1.5*depthoftree,1.5*widthoftree))
  try(plot(partykitctree))
  dev.off()
}




compilefits<-function(Sparameters,fitmodelsavepath,pdfpath=fitmodelsavepath,.progress="text"){
  #library(magick)
  #library(rsvg)
  plyr::llply(Sparameters,function(Sparameter){
    wherefitissaved<-file.path(fitmodelsavepath,paste0(Sparameter$variable,".rda"))
    #print(paste0("Now creating graphs for ",Sparameter$variable),quote=FALSE)
    load(wherefitissaved)
      Sparameter$splits<-
      lapply(Sparameters_i$splits,function(Split){
        if(is.element("fit.model",names(Split))){
          depthoftree<-9.6;
          widthoftree<-9.6;
          try(widthoftree<-length(unique(party::where(Split$fit.model))));
          try(depthoftree<-treedepth(Split$fit.model@tree));
          Split$fit.text<-capture.output(Split$fit.model)
          Split$fit.plotpath<-basename(paste0(tempfile(tmpdir = ".",pattern=paste0(Sparameter$variable,"_")),".pdf"))
          treetopdf(Split$fit.model,file.path(pdfpath,Split$fit.plotpath))
          try(Split$tree<-Split$fit.model@tree[c("nodeID","criterion","terminal","psplit","ssplits","left","right")])
          Split$fit.model<-NULL
        }
        Split
      })
      Sparameter
  },.progress=.progress)
}




compilesamplereports<-function(Sparameters,samplereportssavepath){
  library(magick)
  library(rsvg)
  lapply(Sparameters,function(Sparameter){
    wheresamplereportissaved<-file.path(samplereportssavepath,paste0(Sparameter$variable,".rda"))
    print(paste0("Now creating sample report for ",Sparameter$variable),quote=FALSE)
    load(wheresamplereportissaved)
    if(length(Sparameter$splits)>length(Sparameter$splits)){Sparameter$splits<-c(Sparameter$splits,fakesel=list())}
    Sparameter$splits<-lapply(1:length(Sparameter$splits),function(i){
      c(Sparameter$splits[[i]],ReportonSample$splits[[i]])
    })
    Sparameter
  })}