#' Boxplot of Payments by DRG Code
#'
#' @param df a dataframe of DRG code
#' @param option a string name for variable x in the dataframe df
#'
#' @return A boxplot
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#'
#' @examples
#' drg <- read.scv('data/DRG_data.csv')
#' boxplox.drg(drg, 'Average.Medicare.Payments')
#'
boxplox.drg <- function(df, option = 'Average.Medicare.Payments') {
  # set the default option
  options(scipen = 999)
  df$DRG.Definition <- as.numeric(substring(df$DRG.Definition, 1, 3))

  df %>% ggplot(aes(x = DRG.Definition, y = get(option), group = DRG.Definition)) + # group by DGR code
    geom_boxplot(alpha = 0.5) +
    labs(# add x, y, and title
      x = 'DRG Code',
      y = gsub('\\.', ' ', option),
      title = paste('Boxplot of', gsub('\\.', ' ', option), 'by DRG Code')
    ) +
    scale_y_continuous(trans = 'log10') + # scale y axis
    theme(panel.grid = element_blank()) # remove the background
}
