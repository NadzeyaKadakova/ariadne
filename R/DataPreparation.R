#' Unzip data
#'
#' @description unzipper function what unzip given data in directory given as an
#' argument
#'
#'
#'
#'
#' @param directoryZipFiles directory with zip files
#'
#' @param absolutePathDirectoryToCreate path to directory where to put unzipped files
#' if doesn't exist folder will be created
#' @export
#'
unzipper <- function(directoryZipFiles,
                     absolutePathDirectoryToCreate
){
  listOfFiles <- list.files(path = directoryZipFiles,
                            recursive = TRUE,
                            pattern = "\\.zip$",
                            full.names = T)
  lapply(listOfFiles, function(file){
    folder <- gsub(".zip","", gsub(".*/","", file))

    dir.create(path = paste0(gsub("\\\\",
                                  '/',
                                  absolutePathDirectoryToCreate),
                             "/", folder), recursive = TRUE)

    utils::unzip(file, exdir = paste0(gsub("\\\\",
                                           '/',
                                           absolutePathDirectoryToCreate),
                                      "/", folder))
    return(NULL)
  })
}



#' Prepare data to KM plotting
#'
#' @param directories directories where stored time_to_event.csv
#'
#' @param targetIds target cohort ids to filter
#'
#' @param outcomeIds outcome cohort ids to filter
#'
#' @returns data.table dataframe
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
kaplanMeierPlotPreparationDT <- function(targetIds,
                                         outcomeIds,
                                         directories,
                                         kaplanMeierDataCsv){
  unionAcrossDatabases <- lapply(directories, function(directory){
    if(exists(paste0(gsub("\\\\",
                   '/',directory), "/", kaplanMeierDataCsv))){
    timeToEventTable <- data.table::fread(paste0(gsub("\\\\",
                                          '/',directory), "/", kaplanMeierDataCsv))  %>%
      subset(
        target_id %in% targetIds &
          outcome_id %in% outcomeIds
      )
    }
  })
  data.table::rbindlist(unionAcrossDatabases)
}
