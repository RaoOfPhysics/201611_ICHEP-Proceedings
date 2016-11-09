library(dplyr)
library(gridExtra) # for "arrangeGrob"
library(grid) # for "textGrob"
library(ggplot2)
library(reshape2) # for "melt"

dat <- read.csv(file = "../quant_analysis/final_dataset.csv", header = TRUE)

imp_because_taxes <- melt(as.matrix(dat[c('imp_because_taxes')]))
imp_because_taxes$value <- factor(imp_because_taxes$value, 
                                  levels=c(1, 2, 3, 4, 5))

image1 = ggplot(imp_because_taxes, aes(x = value)) + 
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
    labs(title = "Outreach important because \ntaxes fund research.")

funding_should_support <- melt(as.matrix(dat[c('funding_should_support')]))
funding_should_support$value <- factor(funding_should_support$value, 
                                       levels=c(1, 2, 3, 4, 5))

image2 = ggplot(funding_should_support, aes(x = value)) + 
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
    labs(title = "Funding bodies should provide \nsupport for doing outreach.")

grants_wider_society <- melt(as.matrix(dat[c('grants_wider_society')]))
grants_wider_society$value <- factor(grants_wider_society$value, 
                                     levels=c(1, 2, 3, 4, 5))

image3 = ggplot(grants_wider_society, aes(x = value)) + 
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
    labs(title = "Grant applications should include \ndetails of outreach activities.")

ggsave(file="../201611_ICHEP-Proceedings/figures/figure_01/figure_01.svg",
       arrangeGrob(image1, image2, image3, nrow=1, ncol=3, 
                   top=textGrob("Outreach and research funding",
                                gp = gpar(fontsize = 20))),
       width=10, height=4)