library(dplyr)
library(gridExtra) # for "arrangeGrob"
library(grid) # for "textGrob"
library(ggplot2)
library(reshape2) # for "melt"

dat <- read.csv(file = "../quant_analysis/final_dataset.csv", header = TRUE)

makes_better_scientist <- melt(as.matrix(dat[c('makes_better_scientist')]))
makes_better_scientist$value <- factor(makes_better_scientist$value, 
                                       levels=c(1, 2, 3, 4, 5))

image1 = ggplot(na.omit(makes_better_scientist), aes(x = value)) + 
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
    labs(title = "“[…] makes me a better scientist.”")

feeling_enjoyment <- melt(as.matrix(dat[c('feeling_enjoyment')]))
feeling_enjoyment$value <- factor(feeling_enjoyment$value, 
                                  levels=c(1, 2, 3, 4, 5))

image2 = ggplot(na.omit(feeling_enjoyment), aes(x = value)) + 
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
    labs(title = "Feeling of enjoyment")

job_satisfaction <- melt(as.matrix(dat[c('job_satisfaction')]))
job_satisfaction$value <- factor(job_satisfaction$value, 
                                 levels=c(1, 2, 3, 4, 5))

image3 = ggplot(na.omit(job_satisfaction), aes(x = value)) + 
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
    labs(title = "Job satisfaction")

ggsave(file="../201611_ICHEP-Proceedings/figures/figure_02/figure_02.svg",
       arrangeGrob(image1, image2, image3, nrow=1, ncol=3, 
                   top=textGrob("Benefits of outreach (1)",
                                gp = gpar(fontsize = 20))),
       width=10, height=4)