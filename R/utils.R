#' Some utility functions, mainly used for `add_ad_class`; also contains `adas_plotter` call
#' 
#' 




#A function to return a vector of probabilities based on distributions
adas_class <- function(adas_curr){
  
  #This just helps with the gamma distribution for very low ADAS-Cog which is basically guaranteed to be cognitively normal
  if(adas_curr<2) adas_curr <- 2
  
  #These are the parameters for the gamma distributions (Norm and AD) and normal distribution
  s1 <- 8.41/6.2*1.1; a1 <- 6.2/s1*1.1
  m2 <- 11.5; sd2 <- 4.4*0.67
  s3 <- 38.44/18.5; a3 <- 18.5/s3*1.3
  
  #The densities for a given ADAS-Cog value (adas_curr) for each distribution
  d_norm <- dgamma(adas_curr, shape=a1, scale=s1)
  d_mci <- dnorm(adas_curr, mean=m2, sd=sd2)
  #d_mci <- dgamma(adas_curr, shape=a2, scale=s2)
  d_ad <- dgamma(adas_curr, shape=a3, scale=s3)
  
  #Take the densities and normalize them to 1 to get a vector of probabilities
  ad_class <- c(norm=d_norm, mci=d_mci, ad=d_ad)
  ad_class <- ad_class / sum(ad_class)
  
  return(ad_class)
}



# Some utility code to test the distributions of parameters
dummy_param_plot <- function(){
  
  #The parameters 
  s1 <- 8.41/6.2*1.1; a1 <- 6.2/s1*1.1
  m2 <- 11.5; sd2 <- 4.4*0.67
  s3 <- 38.44/18.5; a3 <- 18.5/s3*1.3
  
  #Make a dummy dataframe
  dat <- data.frame(adas=c(rgamma(1000, shape=a1, scale=s1), rnorm(1000, mean=m2, sd=sd2), rgamma(1000, shape=a3, scale=s3)),
                    class=factor(rep(c("Norm", "MCI", "AD"), each=1000), levels=c("Norm", "MCI", "AD")))
  #Return a plot
  ggplot(dat) + geom_density(aes(x=adas, color=class)) + theme_bw()
}