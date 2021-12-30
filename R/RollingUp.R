#' Roll Up Conditions categories
#'
#' @description creates tables with condition categories
#'
#' @export
rollUpConditions <- function(connection,
                             cdmDatabaseSchema,
                             conditionsIds
                            ) {

  packageName <- getThisPackageName()

  sqlFileName <- "RollUpConditions.sql"

  pathToSql <- system.file("sql",
                           sqlFileName,
                           package = packageName
  )
  sql <- readChar(pathToSql, file.info(pathToSql)$size)

  sqlRendered <- SqlRender::render(
    sql = sql,
    cdmDatabaseSchema = cdmDatabaseSchema,
    conditionsIds = conditionsIds
  )

  rolledUpConditionTable <- DatabaseConnector::querySql(connection = connection,
                                                    sql = sqlRendered,
                                                    snakeCaseToCamelCase = TRUE)
}
#' Roll Up Drug categories
#'
#' @description creates tables with drug categories
#'
#' @param connection DatabaseConnector::connect instance
#' @param cdmDatabaseSchema available cdm db
#' @param drugConceptIds vector of standard concept ids
#' @param `ATC 1st` Boolean argument if TRUE - rolling up to ACT 1st otherwise ACT 2st
#' @export
rollUpDrugs <- function(connection,
                        cdmDatabaseSchema,
                        drugConceptIds,
                        `ATC 1st` = TRUE # ATC 2 by default
) {

  packageName <- getThisPackageName()

  sqlFileName <- "RollUpDrugs.sql"

  pathToSql <- system.file("sql",
                           sqlFileName,
                           package = packageName
  )
  sql <- readChar(pathToSql, file.info(pathToSql)$size)

  sqlRendered <- SqlRender::render(
    sql = sql,
    cdmDatabaseSchema = cdmDatabaseSchema,
    drugConceptIds = drugConceptIds,
    ATC = data.table::fifelse(`ATC 1st`, 'ATC 1st', 'ATC 2nd')
  )

  rolledUpConditionTable <- DatabaseConnector::querySql(connection = connection,
                                                        sql = sqlRendered,
                                                        snakeCaseToCamelCase = TRUE)
}
