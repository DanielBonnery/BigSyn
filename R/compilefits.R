#' Compute depth of a "party" tree
#' @param x a tree
#' @description this function takes the output of the partykit::ctree function and prints the tree into a pdf file, located in the specified folder. It computes the depth and width and tries to create a pdf with the right dimensions.
#' @details recursive function
#' @examples
#' y=iris$Species;x=iris[,-5]
#' partyctree <- party::ctree(y ~ ., data=cbind(y=y,x))
#' treedepth(partyctree@tree)
#' partyctree <- party::ctree(y ~ ., data=cbind(y=y,x))
#' treedepth(partykit::ctree(y ~ ., data=cbind(y=y,x)))
treedepth<-function(x){
  if(is.element("SplittingNode",class(x))|is.element("TerminalNode",class(x))){
  if(x$terminal){1}else{1+max(treedepth(x$right),treedepth(x$left))}
    }else{
    1+depth(x)}}

#' Ctree to pdf graph
#' @param partykitctree: an output of partykit::ctree
#' @param savepath: a file path where to store the pdf of the plot
#' @description this function takes the output of the partykit::ctree function and prints the tree into a pdf file, located in the specified folder. It computes the depth and width and tries to create a pdf with the right dimensions.
#' @examples
#' y=iris$Species;x=iris[,-5]
#' partykitctree <- partykit::ctree(y ~ ., data=cbind(y=y,x))
#' treetopdf(partykitctree,"./x.pdf")
treetopdf<-function(partykitctree,savepath){
  depthoftree<-9.6;
  widthoftree<-9.6;
  try(widthoftree<-width(partykitctree));
  try(depthoftree<-depth(partykitctree));
  pdf(savepath,width=4+3*widthoftree,height=4+max(1.5*depthoftree,1.5*widthoftree))
  try(plot(partykitctree))
  dev.off()
}

#' Save a pdf image of each regression tree grown in the modeling phase and discard useless information
#' 
#' @param Sparameters: a list, that has the same structure than the outputs of
#' @param fitmodelsavepath: a file path where to store the pdf of the plot
#' @param pdfpath where to save the pdfs
#' @param .progress: a string, name of the progress bar to use, see plyr::create_progress_bar
#' @description For each element of save parameters, look at the tree and produces the corresponding pdf.  It also removes all the information that is stored in the ouptut of parykit::Ctree, e.g. the data. It only keeps the tree and the rules to get it.
#' @details Depends on plyr. Partykit output contain all the data that was used to grow the tree. this function removes the unwanted information.
#' @examples
#' y=iris$Species;x=iris[,-5]
#' partykitctree <- partykit::ctree(y ~ ., data=cbind(y=y,x))

compilefits<-function(Sparameters,
                      fitmodelsavepath,
                      pdfpath=fitmodelsavepath,
                      .progress="text"){
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

#' compilesamplereports
#' @param Sparameters: a list, that has the same structure than the outputs of
#' @param samplereportssavepath: a file path where to store the sample reports
#' @description Sample reports are the output of the function ReportonSample
#' @details depends on plyr
#' @seealso ReportonSample
#' @examples
#' y=iris$Species;x=iris[,-5]
#' partykitctree <- partykit::ctree(y ~ ., data=cbind(y=y,x))
compilesamplereports<-function(Sparameters,samplereportssavepath){
  library(magick)
  library(rsvg)
  lapply(Sparameters,function(Sparameter){
    wheresamplereportissaved<-file.path(samplereportssavepath,paste0(Sparameter$variable,".rda"))
    print(paste0("Now creating sample report for ",Sparameter$variable),quote=FALSE)
    load(wheresamplereportissaved)
    if(length(Sparameter$splits)>length(Sparameter$splits)){
      Sparameter$splits<-c(Sparameter$splits,fakesel=list())}
    Sparameter$splits<-lapply(1:length(Sparameter$splits),function(i){
      c(Sparameter$splits[[i]],ReportonSample$splits[[i]])
    })
    Sparameter
  })}