#' Double Axis Graph Function
#'
#' Return the merge of two plots into a single one
#'
#' @param graf1 Plot n 1
#' @param graf2 Plot n 2
#'
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom ggplot2 ggplot_build
#' @importFrom gtable gtable_add_grob
#' @importFrom gtable gtable_add_cols
#' @importFrom grid unit
#'
#' @return graf
#'
#' @examples
double_axis_graph <- function(graf1,graf2){

  # no visible binding for global variable
  name <- r <- unit <- NULL

  # two plots

  graf1 <- graf1

  graf2 <- graf2

  # extract gtable

  gtable1 <- ggplot_gtable(ggplot_build(graf1))

  gtable2 <- ggplot_gtable(ggplot_build(graf2))


  # overlap the panel of 2nd plot on that of 1st plot

  par <- c(subset(gtable1[['layout']], name=='panel', select=t:r))

  graf <- gtable_add_grob(gtable1, gtable2[['grobs']][[which(gtable2[['layout']][['name']]=='panel')]],

                          par['t'],par['l'],par['b'],par['r'])



  ia <- which(gtable2[['layout']][['name']]=='axis-l')

  ga <- gtable2[['grobs']][[ia]]

  ax <- ga[['children']][[2]]

  ax[['widths']] <- rev(ax[['widths']])

  ax[['grobs']] <- rev(ax[['grobs']])

  ax[['grobs']][[1]][['x']] <- ax[['grobs']][[1]][['x']] - unit(1,'npc') + unit(0.15,'cm')

  graf <- gtable_add_cols(graf, gtable2[['widths']][gtable2[['layout']][ia, ][['l']]], length(graf[['widths']])-1)

  graf <- gtable_add_grob(graf, ax, par['t'], length(graf[['widths']])-1, par['b'])

  return(graf)

}
