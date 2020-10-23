#' Create the sample blocks from a posterior matrix
#'
#' @param posterior Matrix of posterior parameters from the adsim package
#' @param n The number of samples per block or a vector of length blocks (default 50)
#' @param blocks the number of blocks (default 10)
#'
#' @export
#'
#' @return A list of single vectors (median parameter values) blocks


make_posterior_blocks <- function(posterior, n=50, blocks=10){

  purrr::map(seq_along(1:blocks), function(ii){



    #Set number of samples per block
    n_curr <- n
    if(length(n)>1) n_curr <- n[ii]

    #Sample indices from posterior and pull the sampled matrix
    .idx <- sample(c(1:nrow(posterior)), n_curr, replace=F)
    posterior_curr <- posterior[sort(.idx),]

    #Take median values of sampled parameters to be returned
    popPars_curr <- apply(posterior_curr,2,median)
    names(popPars_curr) <- colnames(posterior)

    #Add upper and lower bounds of MMSE scores to the
    popPars_curr['bmmse_lb'] <- sample(c(8:15), 1)
    popPars_curr['bmmse_ub'] <- popPars_curr['bmmse_lb'] + sample(c(10:25), 1)
    if(popPars_curr['bmmse_ub']>30) popPars_curr['bmmse_ub'] <- 30

    #Returned sampled median parameters
    return(popPars_curr)
  })
}
