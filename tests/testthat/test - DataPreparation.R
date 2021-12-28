library(testthat)

test_that("Unzipper", {
  ariadne::unzipper(directoryZipFiles = "C:/Users/Alex/D/projects/PIONEER-Odysseus",
                    absolutePathDirectoryToCreate = "C:/Users/Alex/D/projects/PIONEER-Odysseus/testss"

  )
  expect_true(dir.exists("C:/Users/Alex/D/projects/PIONEER-Odysseus/testss"))
})

test_that("kaplanMeierPlotPreparationDT", {
  ariadne::unzipper(

  )

})
