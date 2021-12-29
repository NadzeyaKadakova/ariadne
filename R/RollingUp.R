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
                                                    sql = sqlRendered)
}



#' @export
rollUpDrugs <- function(connection,
                        cdmDatabaseSchema,
                        drugConceptIds
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
    drugConceptIds = drugConceptIds
  )

  rolledUpConditionTable <- DatabaseConnector::querySql(connection = connection,
                                                        sql = sqlRendered)
}
