#' Take the simulated ADAS-Cog scores and probablistically get a cognitive status
#'
#' See `utils.r` for helper function used to calculate probabilities based on ADAS-Cog score
#'
#' @param adas_calc A dataframe with ADAS-Cog scores (named "Adas")
#'
#' @export
#'
#' @return The dataframe with classifications included
#'




add_ad_class <- function(adas_calc){


  adas_calc$ad_class <-

    #For each ADAS-Cog score
    purrr::map_chr(adas_calc$Adas, function(adas){

      #Call `adas_class` to get a vector of probabilities and use `rmultinom` to get a class
      .p <- adas_class(adas)
      .c <- rmultinom(1,1,.p)
      rownames(.c)[.c==1]
    })

  #Cast as a factor and return the dataframe
  adas_calc$ad_class <- factor(adas_calc$ad_class, levels=c("norm", "mci", "ad"))
  return(adas_calc)
}


