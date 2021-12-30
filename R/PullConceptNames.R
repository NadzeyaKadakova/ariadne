#' @param connection DatabaseConnector::connect instance
#' @param cdmDatabaseSchema available cdm db
#' @param covariateIds vector of standard concept ids from covariate table
#' @export
pullUpConceptNames <- function(connection,
                               cdmDatabaseSchema,
                               covariateIds) {
  packageName <- getThisPackageName()

  sqlFileName <- "PullUpConceptNames.sql"

  pathToSql <- system.file("sql",
                           sqlFileName,
                           package = packageName
  )
  sql <- readChar(pathToSql, file.info(pathToSql)$size)

  sqlRendered <- SqlRender::render(
    sql = sql,
    cdmDatabaseSchema = cdmDatabaseSchema,
    covariateIds = covariateIds
  )

  pulledUpConceptNames <- DatabaseConnector::querySql(
    connection = connection,
    sql = sqlRendered,
    snakeCaseToCamelCase = TRUE
  )
}
