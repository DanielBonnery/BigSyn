#' export all tables from an installed R data package to sas7bdat data files in a given library
#' 
#' @param package package to be transformed
#' @param path_to_export_to path to export the data 
#' @param tables tables to extract, if NULL, all tables.
#' @param zip (should the files be zipped (requires utils package))
#' @return nothing
#' @examples 
#' exportRpackagedata_to_csvlibrary("datasets")

exportRpackagedata_to_csvlibrary<-function(package,path_to_export_to=getwd(),tables=NULL,zip=TRUE,newvar=NULL){
  sapply(
    if(is.null(tables)){data(package=package)$results[,"Item"]}else{
      intersect(tables,data(package=package)$results[,"Item"])},
    function(y){
      zz<-get(data(list=y,package=package))
      if(!is.null(newvar)){zz<-cbind(zz,newvar)}
      rownames(zz)<-NULL
      try(write.table(zz,dec = ".",sep=";",quote = F,row.names = FALSE,
                     file.path(path_to_export_to,paste0(y,'.csv'))))
      if(zip){
        try(utils::zip(zipfile = file.path(path_to_export_to,paste0(y,'.zip')),
                       files=file.path(path_to_export_to,paste0(y,'.csv'))
                       ,extras="-j"))
        try(file.remove(file.path(path_to_export_to,paste0(y,'.csv'))))}
    })
}
