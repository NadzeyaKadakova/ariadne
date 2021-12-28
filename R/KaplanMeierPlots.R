#' Creates grid Kaplan-Meier Plot
#'
#' @description
#'
#'
#'
#'
#' @param
#'
#' @param
#'
#'
#' @export
#'
gridKMplot <- function(
  data,
  palette = wesanderson::wes_palette("Darjeeling1", 6 , type = c("continuous")),
  conf.int = TRUE,
  ylab = "Probability",
  xlab = "Time in years",
  ggtheme = ggplot2::theme_bw(),
  gridGroup1,
  gridGroup2,
  savePlots = FALSE,
  directoryToSave,
  censor = FALSE,
  conf.int.style = "ribbon"
){
  listOfPlots <- sapply(c(10, 20, 30), function(year){
    survminer::ggsurvplot(
      data,
      conf.int = TRUE,
      palette = palette,
      ylab  = ylab, #"Probability",
      xlim = c(0, year),
      xlab = xlab, # "Time in years",
      break.time.by = dplyr::if_else(year == 10, 2,
                                     dplyr::if_else(year == 20, 4, 5)),
      ggtheme = ggtheme,
      ncensor.plot = FALSE,
      conf.int.style = conf.int.style,
      censor = censor
    ) + ggplot2::facet_grid(gridGroup1 ~ gridGroup2)
    if(savePlots){
    ggplot2::ggsave(paste0(gsub("\\\\",
                       '/',
                       directoryToSave), "/",
                  gridGroup1, "_", year, "_", gridGroup2,
                  ".jpeg"),
           width = 24, height = 24, units = "cm")
    }
  })
}
