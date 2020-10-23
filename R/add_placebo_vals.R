#' Takes the subject specific ADAS-Cog scores and add placebo modifications, donepezil use, and random noise
#'
#' These modifications add a slight dip in ADAS-Cog scores early on before natural progression begins again.
#' The donepezil use is based on 5mg usage from adsim.  Finally, random noise follows a normal distribution.
#'
#' @param adas_theta The list of dataframes of longitudinal logit-scale ADAS-Cog scores with covariates
#' @param pl_blocks The list of median parameter values made by `make_posterior_blocks`
#'
#' @export
#'
#' @return A final dataframe combining all blocks and indexed by study and subject with placebo modifications
#'
#'





add_placebo_vals <- function(adas_theta, pl_blocks){

  #Begin by iterating over the lists of adas_theta and pl_blocks
  adas_list <-
    lapply(seq_along(1:length(adas_theta)), function(ii){

      #For each block, pull the original parameters and the dataframe of longitudinal ADAS-Cog scores and covariates
      popPars_curr <- pl_blocks[[ii]]
      adas_curr <- adas_theta[[ii]]

      #Residual variance epsilon
      tauResid <- rgamma(1, popPars_curr['kappaEpsilon'],popPars_curr['kappaEpsilon'] * popPars_curr['phiEpsilon']^2)


      #Placebo adjustment

      #Some initializations for the attentuations of placebo
      nSubj <- length(unique(adas_curr$SubjID))
      adas_curr$plac_idx <- adas_curr$ache_idx <- 0

      #Determine number of subjects that get a placebo based attenuation  (set from 0 to 1.0 by 0.2)
      prop_plac <- sample(seq(0,1,0.2), 1)
      .idx_plac <- sample(1:nSubj, floor(nSubj*prop_plac))
      adas_curr$plac_idx[adas_curr$SubjID %in% .idx_plac] <- 1

      #Calculate the current attenuation from `popPars`
      kel <- popPars_curr['kel']
      keq <- kel + popPars_curr['keqMinusKel']
      beta <- - popPars_curr['aucPlacebo'] / (1 / kel - 1 / keq)
      adas_curr$Placebo <- beta * ( exp( -kel * adas_curr$Week) - exp( -keq * adas_curr$Week ) )


      #AChe adjustment

      #Again determine the proportion of subjects taking medication
      prop_ache <- sample(seq(0,1,0.2), 1)
      .idx_ache <- sample(1:nSubj, floor(nSubj*prop_ache))
      adas_curr$ache_idx[adas_curr$SubjID %in% .idx_ache] <- 1

      #Pull anyone who has a baseline MMSE over 25 since they wouldn't be using it
      adas_curr$ache_idx[adas_curr$BMMSE>25] <- 0

      #Calculate the 5mg attenuation
      eStar <- popPars_curr['eStar[1]']
      et50 <- popPars_curr['et50[1]']
      b <- 12 / et50
      eDelta <- - (1 + b) * eStar / b
      adas_curr$eDon5 <- eDelta * adas_curr$Week / (et50 + adas_curr$Week)


      #Update the dataframe

      #Update the conditional logit with the placebo and AChe attenuations
      adas_curr$LogitCondExp <- with(adas_curr, LogitCondExp + plac_idx*Placebo + ache_idx*eDon5)

      #Calculate {0,1} Beta level value of ADAS, sample some random error from tauResid, cast to final ADAS
      adas_curr$Theta <- with(adas_curr, exp(LogitCondExp) / (1 + exp(LogitCondExp)))
      adas_curr$Adas <- with(adas_curr, 70 * rbeta(length(Theta), Theta * tauResid, (1 - Theta) * tauResid))

      #Final prep to get a datagrame with study index
      adas_curr <- data.frame(StudyID=ii, adas_curr)

      return(adas_curr)
    })

  #Combine the list into a single dataframe
  do.call(rbind, adas_list)
}
