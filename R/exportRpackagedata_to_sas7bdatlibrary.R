#' export all tables from an installed R data package to sas7bdat data files in a given library
#' 
#' @param package package to be transformed
#' @param SAS_library_path path to export the data 
#' @return nothing
#' @examples 
#' exportRpackagedata_to_sas7bdatlibrary("datasets")

exportRpackagedata_to_sas7bdatlibrary<-function(package,path_to_export_to=getwd(),tables=NULL){
  sapply(
    if(is.null(tables)){data(package=package)$results[,"Item"]}else{
      intersect(tables,data(package=package)$results[,"Item"])},
    function(y){
      zz<-get(data(list=y,package=package))
      try(haven::write_sas(zz,
                           file.path(path_to_export_to,paste0(y,'.sas7bdat'))))
      if(!is.data.frame(zz)){plyr::l_ply(zz,function(zzz){
        try(haven::write_sas(zzz,file.path(path_to_export_to,paste0(y,'.sas7bdat'))))
      })}
})
}
