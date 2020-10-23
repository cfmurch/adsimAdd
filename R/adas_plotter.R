#' A function to plot the beginning and ending classifications
#'
#'  @param adas_final A dataframe of ADAS-Cog scores and classifications
#'
#'  @export
#'
#'  @return A list of plots, 'bl' for baseline and 'end' for last visit
#'
#'


#The plotter function
adas_plotter <- function(adas_final){

  #Get the baseline and last visit values in separate dataframes
  dat0 <- adas_final[adas_final$Week==0,]
  datX <-
    adas_final %>%
    group_by(StudyID, SubjID) %>%
    filter(Week==max(Week))

  #Return a list of plots
  list(bl=ggplot(dat0) + geom_histogram(aes(x=Adas, fill=ad_class)) + theme_bw(),
       end=ggplot(datX) + geom_histogram(aes(x=Adas, fill=ad_class)) + theme_bw())
}
