#' Add times to the baseline values and calculate the subject-specific ADAS-Cog scores on the logit scale
#'
#'
#'
#' @importFrom simstudy defData addPeriods genData
#' @importFrom dplyr select
#'
#' @export
#'
#' @return A list of studies which each has a long dataframe with the sample times and calculated ADAS-Cog scores
#'
#'




set_times <- function(bl_vars){

  lapply(bl_vars, function(bl_curr){

    #Sample the number of years for the current study, find the maximum number of weeks
    max_years <- sample(c(1,1.5,2,3,4,5), 1)
    max_weeks <- 52*max_years

    #Determine a random number of visits
    #This matters little since there's currently no quadratic component

    #First set the simstudy parameters for the periods (number, mean interval time, interval time variance)
    def1 <- simstudy::defData(varname = "nCount", dist="noZeroPoisson", formula=ceiling(max_years*2))
    def1 <- simstudy::defData(def1, varname="mInterval", dist="nonrandom", formula=max_weeks*0.25)
    def1 <- simstudy::defData(def1, varname="vInterval", dist="nonrandom", formula=0.2)

    #Sample the time using simstudy
    dt <- simstudy::genData(nrow(bl_curr), def1)
    dtPeriod <- simstudy::addPeriods(dt)
    dtPeriod <- dtPeriod[time <= max_weeks]
    dtPeriod <- dplyr::select(dtPeriod, id, "Week"=time)

    #Merge the baseline values and times
    bl_curr <- merge(x=bl_curr, y=dtPeriod, by.x="SubjID", by.y="id")

    #Calculate the ADAS-Cog score at each time point on the logit scale
    bl_curr$LogitCondExp <- with(bl_curr, Eta + Alpha * Week)

    return(bl_curr)

  })
}
