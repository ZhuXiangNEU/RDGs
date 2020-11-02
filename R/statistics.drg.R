#' Statistics Calculation for DRG code
#'
#' This function is to calculate statistics over all of the DRG codes
#' for average Medicare payments
#'
#' @param df a dataframe
#' @param option a string name to tell how to calculate (mean, median, sd)
#'
#' @return a dataframe of statistics calculation by option
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr rename
#'
#' @examples
#' drg <- read.scv('data/DRG_data.csv')
#' statistics.drg(drg, 'sd')
#'
statistics.drg <- function(df, option = 'mean') {
  df %>% mutate(`DRG Code` = as.numeric(substring(DRG.Definition, 1, 3))) %>% # substring the code of DRG
    select(`DRG Code`, Average.Medicare.Payments) %>% # select needed columns
    group_by(`DRG Code`) %>% # group by code
    summarise(option = switch(# decide which function used by option
      option,
      mean = mean(Average.Medicare.Payments),
      median = median(Average.Medicare.Payments),
      sd = sd(Average.Medicare.Payments)
    )) %>%
    rename(Statistics = option) # rename the column name of option
}
