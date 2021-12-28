library(testthat)

test_that("Unzipper", {
  dir <- "C:/Users/Alex/D/projects/Regeneron/ariadne/dataForTests/zipTest"
  zipDir <- "C:/Users/Alex/D/projects/Regeneron/ariadne/dataForTests"
  expect_true(!dir.exists(dir))

  ariadne::unzipper(directoryZipFiles = zipDir,
                    absolutePathDirectoryToCreate = dir

  )
  expect_true(dir.exists(dir))
})



test_that("kaplanMeierPlotPreparationDT", {
  dirs <- list.dirs(path = "C:/Users/Alex/D/projects/Regeneron/ariadne/dataForTests",
                    full.names = T,
                    recursive = F)

  out <- ariadne::kaplanMeierPlotPreparationDT(
    targetIds = c(101:112),
    outcomeIds = c(202:212),
    directories = dirs,
    kaplanMeierDataCsv = "cohort_time_to_event.csv"
  )
 expect_error(ariadne::kaplanMeierPlotPreparationDT(
   targetIds = c(101:112),
   outcomeIds = c(202:212),
   directories = dirs,
   kaplanMeierDataCsv = "cohort_time_to_event.csv"
 ), NA)
 expect_type(out, 'list')
 expect_true(is.data.frame(out))
 expect_gte(nrow(out), 0)

})
