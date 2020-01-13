#' Simple General Transpose function
#' 
#' @param tableA A dataframe
#' @param id1 A list of variables (rows)
#' @param id2 A list of variables (columns of the transposed table)
#' @return A data frame
#' @examples 
#' tableA<-sampledata(TRUE)
#' id1=c("id1a","id1b")
#' id2=c("id2a","id2b")
#' TtableA<-Generaltransposefunctionsimple(tableA,id1,id2)
Generaltransposefunctionsimple<-function(tableA,id1,id2){
  tableA$present<-1
  dcast.data.table(setDT(tableA), 
                   as.formula(paste0(paste(id1,collapse="+"),"~",paste(id2,collapse="+"))),
                   value.var = setdiff(names(tableA),c(id1,id2)))}

#' General Transpose function
#' 
#' @param table A dataframe
#' @param id1 A list of variables (rows)
#' @param id2 A list of variables (columns of the transposed table), id2 can contain as a last element the strint "rn", if the variable rn is an index for the cells formed by the variables listed first in id2
#' @return A list: first element of the list is a dataframe, the transposed version of the orioginal table. Second element is a key to allow back transposition
#' @examples 
#' tableA<-sampledata(TRUE)
#' id1=c("id1a","id1b")
#' id2=c("id2a","id2b")
#' TtableA<-Generaltransposefunction(tableA,id1,id2)
Generaltransposefunction<-function(tableA,id1,id2,origin=deparse(substitute(tableA))){
  #require(reshape2)
  library(data.table)
  require(dplyr)
  origin<-gsub("_","",origin)
  presentindname<-gsub("_","",paste0(origin,".","present"))
  if(nrow(unique(tableA[c(id1,id2)]))!=nrow(tableA[c(id1,id2)])){
    warning("duplicates values in tableA for id1 id2")
    if(is.element("rn",names(tableA))){stop("Error: rn already exist")}
    library(dplyr)
    as.data.frame(do.call(group_by_,c(list(.data=tableA),as.list(c(id1,id2)))) %>%  mutate(rn = paste0("rn",row_number())))->tableA
    id2<-c(id2,"rn")
  }else{
    if(!is.element("rn",names(tableA))){
      tableA$rn="rn1"}}
  rn="rn"
  id2<-unique(c(id2,rn))
  initialnames<-names(tableA)
  initialclasses<-sapply(tableA,class)
  names(tableA)<-gsub(pattern = "_",replacement = "",names(tableA))
  id1<-gsub(pattern = "_",replacement = "",id1)
  id2<-gsub(pattern = "_",replacement = "",id2)
  names(tableA)<-gsub(pattern = ".",replacement = "",names(tableA),fixed=TRUE)
  id1<-gsub(pattern = ".",replacement = "",id1,fixed=TRUE)
  id2<-gsub(pattern = ".",replacement = "",id2,fixed=TRUE)
  names(tableA)<-gsub(pattern = "present",replacement = "presnt",names(tableA))
  id1<-gsub(pattern = "present",replacement = "presnt",id1)
  id2<-gsub(pattern = "present",replacement = "presnt",id2)
  plusorigin<-!is.element(names(tableA),c(id1,id2))
  names(tableA)[plusorigin]<-paste0(origin,".",names(tableA)[plusorigin])
  modifiednames<-names(tableA)
  
  
  
  tableA[id2]<-lapply(tableA[id2],as.factor)
  
  dependent<-setdiff(names(tableA),c(id1,id2))
  factors<-dependent[sapply(tableA[dependent],is.factor)]
  chars<-dependent[sapply(tableA[dependent],is.character)]
  num<-dependent[sapply(tableA[dependent],function(x){!is.factor(x)&!is.character(x)})]
  if(length(num)>0){
  missnum<-as.data.frame(lapply(tableA[num],is.na))
  names(missnum)<-paste0(names(missnum),"missingind")
  tableA<-cbind(tableA,missnum)}

  #convert all characters to factors
  tableA[chars]<-lapply(tableA[chars],as.factor)
  
  #addna
  tableA[c(factors,chars)]<-lapply(tableA[c(factors,chars)],addNA)
  #convert level NA to missing
  tableA[c(factors,chars)]<-
    lapply(tableA[c(factors,chars)],
           function(x){ 
             levels(x)[is.na(levels(x))]<-"missing"
             x
           })
  #Save factors initial levels
  initiallevels<-lapply(tableA[sapply(tableA,is.factor)],levels)
  #Add L to all levels of all variables of id2
  tableA[id2]<-lapply(tableA[id2],function(x){levels(x)<-paste0("L",stringr::str_replace_all(levels(x),"[^[:alnum:]]",""));x})#1:nlevels(x));x})
  
  #Create presence indicator
  tableA[presentindname]<-1
  TtableA<-as.data.frame(dcast.data.table(as.data.table(tableA), 
                                          as.formula(paste0(paste(id1,collapse="+"),"~",paste(id2,collapse="+"))),
                                          value.var = setdiff(names(tableA),c(id1,id2))))
  TtableA[setdiff(names(TtableA)[sapply(TtableA,is.factor)],id1)][is.na(TtableA[setdiff(names(TtableA)[sapply(TtableA,is.factor)],id1)])]<-"missing"
  TtableA[sapply(TtableA,is.numeric)][is.na(TtableA[sapply(TtableA,is.numeric)])]<-0
  list(TtableA=as.data.frame(TtableA),
       key=list(id1=id1,id2=id2, rn=rn,
                num=num,
                origin=origin,
                chars=chars,
                factors=factors,
                initiallevels=initiallevels,
                initialclasses=initialclasses,
                initialnames=initialnames, 
                modifiednames=modifiednames,
                presentindname=presentindname
       ))  
}




#' General Reverse Transpose function
#' 
#' @param table A dataframe
#' @param key A list of variables (columns of the transposed table)
#' @return A list: first element of the list is a dataframe, the transposed version of the orioginal table. Second element is a key to allow back transposition
#' @examples 
#' data(tableA);data(TtableA);data(XKA);key<-XKA$key
#' RtableA=GeneralReversetransposefunction(TtableA,key)
#' ordertableA <-do.call(order,tableA[c(id1,id2)])
#' orderRtableA<-do.call(order,RtableA[c(id1,id2)])
#' identical(nrow(tableA),nrow(RtableA))
#' identical(lapply(tableA,class),lapply(RtableA,class))
#' identical(tableA[ordertableA,],RtableA[orderRtableA,])
#' identical(names(tableA),names(RtableA))
#' all (lapply(names(tableA),function(x){identical(tableA[ordertableA,x],RtableA[orderRtableA,x])}))
GeneralReversetransposefunction<-function (TtableA, key) {
  variables <- setdiff(unique(get_var(names(TtableA))), key$id1)
  cellrns <- setdiff(unique(get_cellrn(names(TtableA))), "")
  names(cellrns) <- cellrns
  names(key$id2) <- key$id2
  
  #This transposition has the drawback of converting all factors to character.
  #TtableA2<-   eval(parse(text=paste0("melt.data.table(as.data.table(TtableA), measure = patterns(",paste(paste0("'^",variables,"'"),collapse=","),"), value.name = variables)")))
  
  #This is more complicated, (more steps)
  L1<-do.call(paste,c(lapply(cellrns, function(x) {
    var2 <- names(TtableA)[get_cellrn(names(TtableA)) == x]
    names(var2) <- get_var(var2)
    var2 <- var2[order(names(var2))]
    paste0(x,"=list(list(", paste(paste(names(var2),var2, sep = "="), collapse = ","), "))")}),list(sep=",")))
  L2=paste0("list(", paste(key$id1, collapse = ","), ")")
  print(paste0(Sys.time()," ---- 1. I am converting to data.table"),quote=FALSE)
  TtableA <- eval(parse(text=paste0("as.data.table(TtableA)[,list(",L1,"),by=",L2,"]")))
  print(paste0(Sys.time()," ---- 2. I am melting, so far object size=",object.size(TtableA) , " bytes"),quote=FALSE)
  
  TTtableA <- melt.data.table(TtableA, id = key$id1, variable.name = "id2", 
                              measure = cellrns, na.rm = FALSE)
  rm(TtableA,L1,L2);gc()
  L1<-paste0("list(", paste(key$id1,collapse = ","), ",id2)")
  print(paste0(Sys.time()," ---- 3. I am creating new variables for transposition by splitting the group name, so far:",object.size(TTtableA)),quote=FALSE)
  #TTtableA[, `:=`(key$id2, as.data.table(do.call(rbind, strsplit(levels(id2)[id2],"_")))), by = eval(parse(text = L1))]
  TTtableA[, `:=`(key$id2, tstrsplit(levels(id2)[id2],"_",fixed=TRUE)), by = eval(parse(text = L1))]
  
  print(paste0(Sys.time()," ---- 4. I am starting the transposition, so far object size=",object.size(TTtableA) , " bytes"),quote=FALSE)
  TTtableA <- as.data.frame(TTtableA[, `:=`(eval(parse(text = paste0("c(", 
                                                                     paste(paste0("'", variables, "'"), collapse = ","), ")"))), 
                                            lapply(variables, function(x) {
                                              unlist(lapply(value, function(y) {
                                                y[[eval(x)]]
                                              }))
                                            })), ][, -c("value", "id2")])
  print(paste0(Sys.time()," ---- 5. I am cleaning up the missing and all"),quote=FALSE)
  TTtableA[[key$presentindname]][is.na(TTtableA[[key$presentindname]])] <- 0
  TTtableA <- TTtableA[TTtableA[[key$presentindname]] == 1,]
  missingind <- grep("missingind", names(TTtableA), value = TRUE)
  numvar <- gsub("missingind", "", missingind)
  TTtableA[missingind][is.na(TTtableA[missingind])] <- TRUE
  TTtableA[numvar] <- lapply(numvar, function(x) {
    y <- TTtableA[[x]]
    y[TTtableA[[paste0(x, "missingind")]]] <- NA
    y
  })
  TTtableA[intersect(key$id2, names(key$initiallevels))] <- lapply(intersect(key$id2, 
                                                                             names(key$initiallevels)), function(x) {
                                                                               y = TTtableA[[x]]
                                                                               if(is.factor(y)){levels(y) <- substr(levels(y),2,nchar(levels(y)))}else{#key$initiallevels[[x]]
                                                                               if(is.character(y)){y <- substr(y,2,nchar(y))}}
                                                                               y
                                                                             })
  TTtableA[sapply(TTtableA, is.factor)] <- lapply(TTtableA[sapply(TTtableA, is.factor)], droplevels, "missing")
  if (any(is.element(names(TTtableA), key$chars))) {
    TTtableA[intersect(names(TTtableA), key$chars)] <- 
      lapply(TTtableA[intersect(names(TTtableA), key$chars)], function(x) {levels(x)[x]})
  }
  TTtableA <- TTtableA[intersect(key$modifiednames, names(TTtableA))]
  names(TTtableA) <- key$initialnames[sapply(names(TTtableA), 
                                             function(x) {
                                               which(key$modifiednames == x)
                                             })]
  pb2 <- setdiff(names(TTtableA), key$initialnames)
  if (length(pb2) > 0) {
    print(paste0(" -- pb: missing ", paste(pb2, collapse = ", ")))
  }
  TTtableA[setdiff(intersect(key$initialnames, names(TTtableA)), key$rn)]
}





#' General Reverse Transpose function with split
#' 
#' @param table A dataframe
#' @param key A list of variables (columns of the transposed table)
#' @return A list: first element of the list is a dataframe, the transposed version of the orioginal table. Second element is a key to allow back transposition
#' @examples 
#' data(tableA);data(TtableA);data(XKA);key<-XKA$key
#' RtableA=GeneralReversetransposefunctiondecoupe(TtableA,key,10)

GeneralReversetransposefunctiondecoupe<-function(.data,key,nrowmax=10000){
  temp.files<-lapply(0:(nrow(.data)%/%nrowmax),function(ll){
    zozo=tempfile()
    synl<-BigSyn::GeneralReversetransposefunction(.data[((ll*nrowmax)+1):min(nrow(.data),((ll+1)*nrowmax)),],key)
    save(synl,file=zozo)
    zozo
  })
do.call(rbind,lapply(temp.files,function(x){get(load(x))}))
}

