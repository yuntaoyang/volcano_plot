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
t_log2FC <- 1 # threshold of log2FC
t_FDR <- 0.1 # threshold of FDR
x <- data$logFC # x axis
y <- -log10(data$FDR) # y axis
color <- data$significant # color based on up/down/ns
label <- data$gene_name # label some gene
xint <- 1 # x intercept log2FC
yint <- 1 # y intercept -log10(FDR)
legend <- FALSE # show legend or not
xlim <- 5 # set up x axis
title <- "A vs. B" # title
filename <- "example.pdf" # file name
width <- 5 # plot width
height <- 5 # plot height

#---- volcano plot function ----------------------------------------------------
volcano_plot <- function(data,x,y,color,label,t_log2FC,t_FDR,legend,xlim,title,filename,width,height){
  ggplot(data, aes(x = x, y = y, color = color, label = label)) +
    theme_clean() + 
    scale_color_manual(values=c("blue3", "grey", "red3")) +
    geom_vline(xintercept=0) +
    geom_vline(xintercept=(t_log2FC), linetype=3, colour="grey60") +
    geom_vline(xintercept=(-t_log2FC), linetype=3, colour="grey60") +
    geom_hline(yintercept=(-log10(t_FDR)), linetype=3, colour="grey60") +
    geom_point(show.legend=legend) +
    geom_label_repel(force = 1, size = 3, box.padding = 0.5, 
                     point.padding = 0.3, show.legend=FALSE) +
    xlim(c(-xlim, xlim)) +
    theme_classic() + 
    xlab("log2(FC)") + 
    ylab("-log10(FDR)") +
    ggtitle(title)
  ggsave(filename, width = width, height = height)
}

#---- make volcano plot --------------------------------------------------------
volcano_plot(data,x,y,color,label,t_log2FC,t_FDR,legend,xlim,title,filename,width,height)

