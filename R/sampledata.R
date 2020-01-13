#' Sample data for transposition
#' @param transposingvariables a boolean. If TRUE, transposing  id variables are created.
#' @return a data frame with id variables, numeric, factor and character variables.
sampledata<-function(transposingvariables=TRUE){
  set.seed(1)
  N<-1000;
  if(transposingvariables){
    tableA<-data.frame(id1a=as.factor(sample(30,N, replace=TRUE)),
                       id1b=as.factor(sample(4,N, replace=TRUE)),
                       id2a=sample(letters[1:3],N,replace=TRUE),
                       id2b=sample(letters[1:4],N,replace=TRUE),
                       cont1=rchisq(N,1),
                       factor1=as.factor(sample(letters[1:3],N,replace=TRUE)),
                       factor2=as.factor(sample(letters[1:4],N,replace=TRUE)),
                       char1=sample(letters[1:3],N,replace=TRUE),
                       char2=sample(letters[1:4],N,replace=TRUE),stringsAsFactors=FALSE)
    tableA$cont2<-10-5*tableA$cont1+rnorm(tableA$cont1)
    tableA$cont3<-1*((tableA$cont1-mean(tableA$cont1)+rnorm(tableA$cont1))>0)
    model.matrix(tableA$cont1~0+tableA$factor1)->MM
    tableA$cont4<-MM%*%rt(ncol(MM),1)
    
    
    #Add missing
    tableA[5:12]<-lapply(tableA[5:12], function(x){x[sample(N,100)]<-NA;x})}
  else{
    tableA<-data.frame(id1=as.factor(1:N),
                       cont1=rchisq(N,1),
                       cont2=rchisq(N,1),
                       factor1=as.factor(sample(letters[1:3],N,replace=TRUE)),
                       factor2=as.factor(sample(letters[1:4],N,replace=TRUE)),
                       char1=sample(letters[1:3],N,replace=TRUE),
                       char2=sample(letters[1:4],N,replace=TRUE),stringsAsFactors=FALSE)
    #Add missing
    tableA[2:7]<-lapply(tableA[2:7], function(x){x[sample(N,10)]<-NA;x})}
  tableA}

#' Transposed sample data. 
#' 
#' @details
#' Tsampledata(x) is Generaltransposefunction(Tsampledata(x))
#' @param transposingvariables a boolean. If TRUE, stransposing  id variables are created.
#' @return a data frame with id variables, numeric, factor and character variables.
Tsampledata<-function(transposingvariables=TRUE){
  tableA<-sampledata(transposingvariables)
  if(transposingvariables){
  id1=c("id1a","id1b");id2=c("id2a","id2b")
  TKtableA<-Generaltransposefunction(tableA,id1,id2,origin="AA")
  }else{
  id1="id1";id2=character(0)
  TKtableA<-Generaltransposefunction(tableA,id1,id2,origin="AA")}
  
  c(TKtableA,
    list(variables=names(TKtableA$TtableA),
         id1=id1,id2=id2))}



#' Transposed sample data. 
#' 
#' @details
#' Tsampledata(x) is Generaltransposefunction(Tsampledata(x))
#' @param transposingvariables a boolean. If TRUE, stransposing  id variables are created.
#' @return a data frame with id variables, numeric, factor and character variables.
TTsampledata<-function(transposingvariables=TRUE){
  TKtableA<-Tsampledata(transposingvariables)
  GeneralReversetransposefunction(TKtableA$tableA,TKtableA$key)}
  