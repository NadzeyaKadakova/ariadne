library(testthat)

test_that("Roll Up Drugs", {
  ids <- c(19069425, #nimesulide
           1713332 #amoxicillin
           )
  cdmDatabaseSchema = "cdm_531"
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = "testnode.arachnenetwork.com/synpuf_110k",
    user = Sys.getenv("ohdsi_password"),
    password = Sys.getenv("ohdsi_password"),
    port = "5441"
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  t <- ariadne::rollUpDrugs(
    connection = conn,
    cdmDatabaseSchema = cdmDatabaseSchema,
    drugConceptIds = ids,
    `ATC 1st` = FALSE
  )
  expect_equal(dim(t), c(2, 4))




})


test_that("Roll Up Condition", {
  ids <- c(4182210, #dementia
           255848   #pneumonia
  )
  cdmDatabaseSchema = "cdm_531"
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = "testnode.arachnenetwork.com/synpuf_110k",
    user = Sys.getenv("ohdsi_password"),
    password = Sys.getenv("ohdsi_password"),
    port = "5441"
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  t <- ariadne::rollUpConditions(
    connection = conn,
    cdmDatabaseSchema = cdmDatabaseSchema,
    conditionsIds = ids
  )
  expect_equal(dim(t), c(2, 4))

  expect_equal(t$CATEGORY_NAME[1], "Respiratory disease")

  expect_equal(t$CATEGORY_NAME[2], "Mental disease")

})
