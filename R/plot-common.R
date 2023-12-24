

#' Add time series event markers to a timeseries plot.
#'
#' The x axis must be a date.
#'
#' @param events `r interfacer::idocument(geom_events, events)`
#' @param event_label_size how big to make the event label
#' @param event_label_y where to place the event label (0 or Inf usually)
#' @param event_label_colour the event label colour
#' @param event_line_colour the event line colour
#' @param event_fill_colour the event area fill
#' @param ... not used
#' @importFrom rlang %||%
#'
#' @return a set of geoms for a timeseries.
#' @export
geom_events = function(
    events = i_events,
    event_label_size=7,
    event_label_y=Inf,
    event_label_colour = "black",
    event_line_colour = "grey50",
    event_fill_colour = "grey50",
    ...
) {
  events = interfacer::ivalidate(events)
  rects = events %>% dplyr::filter(!is.na(end))
  lines = events %>% dplyr::filter(is.na(end))
  return(list(
    ggplot2::geom_rect(data=rects,mapping=ggplot2::aes(xmin=start,xmax=end),inherit.aes = FALSE,ymin=-Inf,ymax=Inf,fill=event_fill_colour,colour=NA,alpha=0.25),
    ggplot2::geom_vline(data=lines,mapping=ggplot2::aes(xintercept=start),colour=event_line_colour,show.legend = FALSE),
    ggrepel::geom_text_repel(
      ggplot2::aes(x=start, y=event_label_y, label=label),data=events, hjust=-0.1,vjust=-0.1, angle=90, show.legend = FALSE,box.padding=0.05,inherit.aes = FALSE,
      size=(event_label_size/ggplot2::.pt/(96/72)), colour=event_label_colour)
  ))
}




