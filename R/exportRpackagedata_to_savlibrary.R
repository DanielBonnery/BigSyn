#' export all tables from an installed R data package to sav data files in a given library
#' 
#' @param package package to be transformed
#' @param path_to_export_to path to export the data 
#' @param tables tables to extract, if NULL, all tables.
#' @param zip (should the files be zipped (requires utils package))
#' @return nothing
#' @examples 
#' exportRpackagedata_to_csvlibrary("datasets")

exportRpackagedata_to_savlibrary<-function(package,path_to_export_to=getwd(),tables=NULL,zip=TRUE){
  sapply(
    if(is.null(tables)){data(package=package)$results[,"Item"]}else{
      intersect(tables,data(package=package)$results[,"Item"])},
    function(y){
      try(haven::write_sav(get(data(list=y,package=package)),
                     file.path(path_to_export_to,paste0(y,'.sav'))))
      if(zip){
        try(utils::zip(zipfile = file.path(path_to_export_to,paste0(y,'.zip')),
                       files=file.path(path_to_export_to,paste0(y,'.sav'))))
        try(file.remove(file.path(path_to_export_to,paste0(y,'.sav'))))}
    })
}
