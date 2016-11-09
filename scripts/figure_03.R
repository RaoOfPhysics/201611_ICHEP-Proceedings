library(dplyr)
library(gridExtra) # for "arrangeGrob"
library(grid) # for "textGrob"
library(ggplot2)
library(reshape2) # for "melt"

dat <- read.csv(file = "../quant_analysis/final_dataset.csv", header = TRUE)

attracts_funding <- melt(as.matrix(dat[c('attracts_funding')]))
attracts_funding$value <- factor(attracts_funding$value, 
                                 levels=c(1, 2, 3, 4, 5))

image1 = ggplot(na.omit(attracts_funding), aes(x = value)) + 
    geom_bar(stat="count", colour="white") + 
    geom_text(stat="count", aes(label = ..count..,
                                y=..count..), vjust=-0.5) +
    scale_x_discrete(name = NULL,
                     limits = c(1:5), 
                     drop = FALSE, 
                     labels = c("1" = "1\nStrongly Disagree",
                                "2" = "2", "3" = "3", "4" = "4", 
                                "5" = "5\nStrongly Agree")) +
    
    ylim(0, 160) +
    theme_bw() + 
    theme(plot.margin=unit(c(10,20,10,20),"pt"),
          axis.text.x = element_text(size=8),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 10)) + 
    labs(title = "Outreach participation attracts research funding.")

advances_career <- melt(as.matrix(dat[c('advances_career')]))
advances_career$value <- factor(advances_career$value, 
                                levels=c(1, 2, 3, 4, 5))

image2 = ggplot(na.omit(advances_career), aes(x = value)) + 
    geom_bar(stat="count", colour="white") + 
    geom_text(stat="count", aes(label = ..count..,
                                y=..count..), vjust=-0.5) +
    scale_x_discrete(name = NULL,
                     limits = c(1:5), 
                     drop = FALSE, 
                     labels = c("1" = "1\nStrongly Disagree",
                                "2" = "2", "3" = "3", "4" = "4", 
                                "5" = "5\nStrongly Agree")) +
    
    ylim(0, 160) +
    theme_bw() + 
    theme(plot.margin=unit(c(10,20,10,20),"pt"),
          axis.text.x = element_text(size=8),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 10)) + 
    labs(title = "Outreach participation advances my career.")

collaboration_opportunities <-
    melt(as.matrix(dat[c('collaboration_opportunities')]))
collaboration_opportunities$value <- factor(collaboration_opportunities$value, 
                                            levels=c(1, 2, 3, 4, 5))

image3 = ggplot(na.omit(collaboration_opportunities), aes(x = value)) + 
    geom_bar(stat="count", colour="white") + 
    geom_text(stat="count", aes(label = ..count..,
                                y=..count..), vjust=-0.5) +
    scale_x_discrete(name = NULL,
                     limits = c(1:5), 
                     drop = FALSE, 
                     labels = c("1" = "1\nStrongly Disagree",
                                "2" = "2", "3" = "3", "4" = "4", 
                                "5" = "5\nStrongly Agree")) +
    
    ylim(0, 160) +
    theme_bw() + 
    theme(plot.margin=unit(c(10,20,10,20),"pt"),
          axis.text.x = element_text(size=8),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 10)) + 
    labs(title = "Outreach participation fosters collaborative.")

research_new_ways <- melt(as.matrix(dat[c('research_new_ways')]))
research_new_ways$value <- factor(research_new_ways$value, 
                                  levels=c(1, 2, 3, 4, 5))

image4 = ggplot(na.omit(research_new_ways), aes(x = value)) + 
    geom_bar(stat="count", colour="white") + 
    geom_text(stat="count", aes(label = ..count..,
                                y=..count..), vjust=-0.5) +
    scale_x_discrete(name = NULL,
                     limits = c(1:5), 
                     drop = FALSE, 
                     labels = c("1" = "1\nStrongly Disagree",
                                "2" = "2", "3" = "3", "4" = "4", 
                                "5" = "5\nStrongly Agree")) +
    
    ylim(0, 160) +
    theme_bw() + 
    theme(plot.margin=unit(c(10,20,10,20),"pt"),
          axis.text.x = element_text(size=8),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 10)) + 
    labs(title = "Outreach participation shapes the direction of research.")

ggsave(file="../201611_ICHEP-Proceedings/figures/figure_03/figure_03.svg",
       arrangeGrob(image1, image2, image3, image4, nrow=2, ncol=2, 
                   top=textGrob("Benefits of outreach (2)",
                                gp = gpar(fontsize = 20))),
       width=10, height=8)