#' Set the subject-specific random intercepts and slopes for
#'
#' @param popPars_curr The list of sampled population parameters taken from `make_posterior_blocks`
#'
#' @return Another list with the subject-specific values
#'
#' @export
#'
#' @import adsim
#' @importFrom dplyr mutate, if_else




set_subject_vars <- function(pl_blocks){

  #Map across the list
  purrr::map(pl_blocks, function(popPars_curr){

    #Initial population-level parameters (prior to covariate adjustment)

    #Pull between 100 and 150 subjects
    nSubj <- sample(c(100:150), 1)

    #Sample population-level means for intercept (Eta) and slopes (Alpha) for the current block
    muEta <- rnorm(1, popPars_curr['nuEta'], popPars_curr['psiEta'] )
    muAlpha <- rnorm(1, popPars_curr['nuAlpha'], popPars_curr['psiAlpha'] )

    #Within study SD of muEta (the intercept) - poputlation-level
    tauEta <- rgamma(1, popPars_curr['kappaEta'], popPars_curr['kappaEta'] * popPars_curr['phiEta']^2)
    sigmaEta <- 1 / sqrt(tauEta)

    #Within study SD of muAlpha (the slope) - we sample only once so it's for a single study
    tauAlpha <- rgamma(1, popPars_curr['kappaAlpha'], popPars_curr['kappaAlpha'] * popPars_curr['phiAlpha']^2)
    sigmaAlpha <- 1 / sqrt(tauAlpha)


    #Sampling subject covariates

    #Sample the baseline MMSE for all subjects
    #Note MMSE does not need to be an integer
    bmmse <- runif(nSubj, popPars_curr['bmmse_lb'], popPars_curr['bmmse_ub']) #Sampling based on the randomly assigned MMSE

    #Sample APOE, Age, Sex - this uses the stock functions of adsim and pulls one parameter for each subjects
    apo <- adsim:::.simApoE(nSubj, popPars_curr)
    age <- adsim:::.simAge(nSubj, popPars_curr, apo)
    gender <- adsim:::.simGen(nSubj, popPars_curr)


    #Adjusting the population-level mean intercept/slope based on subject covariates

    #Adjust the population intercept mmean (relies only on MMSE)
    muEtaAdj <- muEta + popPars_curr['lambdaEtaBMMSE'] * (bmmse - 21)

    #Adjust the population-level mean for slopes (relies on MMSE, age, APOE4 counts, sex)
    muAlphaAdj <- muAlpha + popPars_curr['lambdaAlphaBMMSE'] * (bmmse - 21) +
      popPars_curr['lambdaAlphaAge'] * (age -75) +
      popPars_curr['lambdaAlphaApo1'] * (apo==1) +
      popPars_curr['lambdaAlphaApo2'] * (apo==2) +
      popPars_curr['lambdaAlphaGen'] * gender


    #With the covariate adjusted means for the random intercepts and slopes, you can now sample an intercept and slope for each subject
    eta <- rnorm(nSubj, muEtaAdj, sigmaEta)
    alpha <- rnorm(nSubj, muAlphaAdj, sigmaAlpha)

    #Combine the subject covariates and intercepts/slopes
    baseline <- data.frame(SubjID=c(1:nSubj), BMMSE=bmmse, apo=apo, age=age, gender=gender, Eta=eta, Alpha=alpha)



    #Adjustments for high bmmse - this is an adjustment not included in adsim

    #Attenuate the subject-specific intercept for anyone with baseline MMSE over 26
    baseline <- dplyr::mutate(baseline,
                              Eta=dplyr::if_else(bmmse>26, Eta-abs(Eta*0.15*(bmmse-26)), Eta))

    #Attenuate the subject-specific slope for anyone with baseline MMSE over 27.5
    baseline <- dplyr::mutate(baseline,
                              Alpha=dplyr::if_else(bmmse>27.5, Alpha-abs(Alpha*0.025*(bmmse-27.5)), Alpha))


    #Return the dataframe with the subject covariate, intercepts and slopes
    return(baseline)

  })

}

