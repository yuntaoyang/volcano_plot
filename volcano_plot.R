#---- set up libraries----------------------------------------------------------
library(readr)
library(dplyr)
library(io)
library(ggplot2)
library(ggthemes)
library(ggrepel)

#---- read files ---------------------------------------------------------------
data <- read.csv("data.csv")

#---- set up parameters --------------------------------------------------------
xint <- 1 # x intercept
yint <- 1 # y intercept
legend <- FALSE # show legend or not
xlim <- 5 # set up x axis
xlab <- "log2(FC)" # x
ylab <- "-log10(FDR)" # y
title <- "A vs. B" # title
filename <- "example.pdf" # file name
width <- 5 # plot width
height <- 5 # plot height

#---- volcano plot -------------------------------------------------------------
ggplot(data, aes(x = logFC, y= -log10(FDR), color = significant, label = gene_name)) +
    theme_clean() + 
    scale_color_manual(values=c("blue3", "grey", "red3")) +
    geom_vline(xintercept=0) +
    geom_vline(xintercept=(xint), linetype=3, colour="grey60") +
    geom_vline(xintercept=(-xint), linetype=3, colour="grey60") +
    geom_hline(yintercept=(yint), linetype=3, colour="grey60") +
    geom_point(show.legend=legend) +
    geom_label_repel(force = 1, size = 3, box.padding = 0.5, 
                     point.padding = 0.3, show.legend=FALSE) +
    xlim(c(-xlim, xlim)) +
    theme_classic() + 
    xlab(xlab) + 
    ylab(ylab) +
    ggtitle(title)
ggsave(filename, width = width, height = height)



