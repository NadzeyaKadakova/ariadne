library(testthat)

test_that("Pull Up Concept Names", {
  ids <- c(
    19069425, # nimesulide
    1713332 # amoxicillin
  )
  cdmDatabaseSchema <- "cdm_531"
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = "testnode.arachnenetwork.com/synpuf_110k",
    user = Sys.getenv("ohdsi_password"),
    password = Sys.getenv("ohdsi_password"),
    port = "5441"
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  t <- ariadne::pullUpConceptNames(
    connection = conn,
    cdmDatabaseSchema = cdmDatabaseSchema,
    covariateIds = ids
  )
  expect_equal(dim(t), c(2, 2))

  expect_equal(t$conceptName[2], "nimesulide")
})
