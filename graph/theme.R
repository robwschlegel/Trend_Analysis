###############################################################################
### "graph/theme.R"
## DESCRIPTION: This is a custom ggplot2 theme based on theme_bw()
## USAGE: Add this to the first line of a ggplot2 code chunk with "+ bw_update +"
## ARGUMENTS: The (data) must have columns labeled "site" and "src"
## DETAILS:
## VALUE:
## AUTHORS(S): AJ Smit
## REFERENCE(S):
## EXAMPLE(S):
## DEPENDS: 
library(ggplot2)
library(grid)
## USED BY: "figures.R"
##############################################################################

bw_update <- theme_bw() +
                    theme(plot.background = element_blank(),
                          panel.background = element_rect(colour = NA, fill = NA),
                          panel.border = element_rect(colour = "black",
                                                      fill = NA, size = 0.8),
                          panel.grid.minor = element_line(colour = NA),
                          panel.grid.major = element_line(colour = "black",
                                                          size = 0.2,
                                                          linetype = "dotted"),
                          axis.title = element_text(size = 8),
                          axis.text = element_text(size = 8),
                          axis.ticks = element_line(size = 0.5),
                          axis.ticks.length = unit(2, "mm"),
                          #legend.position = "top",
                          #legend.direction = "horizontal",
                          legend.title = element_text(size = 8),
                          legend.text = element_text(size = 8),
                          legend.key = element_blank(),
                          legend.key.height = unit(0.44, "cm"),
                          #legend.key.width = unit(0.30, "cm"),
                          legend.background = element_blank(),
                          plot.title = element_text(size = 8, hjust = 0),
                          strip.background = element_blank(),
                          strip.text = element_text(size = 8))
