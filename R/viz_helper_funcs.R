
require(ggplot2)

#' Create a Grid of Plots
#'
#' @eval paste("asdf","wasd",sep=" ")
multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated form # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols,
                     nrow = ceiling(numPlots / cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout),
                                               ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]],
            vp = viewport(
              layout.pos.row = matchidx$row,
              layout.pos.col = matchidx$col
            ))
    }
  }
}

#'
# Custom Colors -------------------------
color_white <- "#fbf7f2"
color_black <- "#16151a"

# Custom ggplot2 Theme: Alex F. 04 ----------------------
theme_af04 <- function() {
  theme_classic() +
    theme (
      plot.title = element_text(hjust = 0.5),
      plot.background = element_rect(fill = color_white),
      panel.background = element_rect(
        fill = color_white,
        size = 0.5,
        linetype = "solid"
      ),
      legend.background = element_rect(fill = color_white),
      text = element_text(size = 18)
    )
}

#' Associate a theme with this project
#'
#' @examples
#' theme_project <- theme_classic()
#'
#' \dontrun{
#' theme_project <- theme_classic()
#' }
theme_project <- theme_af04

