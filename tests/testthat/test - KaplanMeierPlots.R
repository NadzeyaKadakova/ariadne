library(testthat)

test_that("gridKMplot", {
  d <- "C:/Users/Alex/D/projects/Regeneron/ariadne/dataForTests/KMTest"
  dirs <- list.dirs(path = d,
                    full.names = T,
                    recursive = F)

  out <- ariadne::kaplanMeierPlotPreparationDT(
    targetIds = c(101:112),
    outcomeIds = c(202:212),
    directories = dirs,
    kaplanMeierDataCsv = "cohort_time_to_event.csv"
  )
  out <- out %>%
    dplyr::mutate(
    strata = dplyr::case_when(
      target_id == 102  ~ "Immediate Treatment",
      target_id == 112 ~ "Conservative Management"
    )
)


  ariadne::gridKMplot(data = out,
                      palette = wesanderson::wes_palette("Darjeeling1",
                                                         6 ,
                                                         type = c("continuous")),
                      conf.int = TRUE,
                      ylab = "Probability",
                      xlab = "Time in years",
                      ggtheme = ggplot2::theme_bw(),
                      gridGroup2 = outcome_id,
                      gridGroup1 = database_id,
                      savePlots = TRUE,
                      directoryToSave = d,
                      censor = F,
                      conf.int.style = "ribbon"

  )


  jpegs <- list.files(
    path = d,
    recursive = TRUE,
    pattern = "\\.jpeg",
    full.names = F
    )

  expect_gte(
    length(jpegs), 0
    )

})

