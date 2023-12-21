library(tidymodels)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(tidyr)
load("data/offers_censored.RData")
source("./helpful_functions.R")
#install.packages("cowplot")

#####
subset_offers <- subset(offers, AppYear!=2023)




# bar plot illustrating the class imbalance
#in the target variable 
ggplot(subset_offers, aes(x = Status, fill = Status)) +
  geom_bar() +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw() +coord_flip()+
  labs(title = "Class Imbalance in Status",
       x = "Status",
       y = "Count")

#bar plot illustrating the Responses of Enrolled 
#and Non-enrolled Individuals
ggplot(subset_offers, aes(x = Response, fill = Status)) +
  geom_bar() +scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw() +labs(title="Responses by Enrollement Status", x = "Response",
                   y = "Count")


#Bar Plot illustrating the First Source of Information about the
#University among Enrolled and Non-enrolled Individuals

ggplot(subset_offers, aes(x = HowFirstHeard, fill = Status)) +
  geom_bar(position = "fill") +
  theme_bw() +scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs( title="Enrollement Status based on 'How did you first heard about RSM'",
        x = "HowFirstHeard", y = "Y-axis Label")



#bar plots depicting each demographic variable
#and their Influence on Enrollment Status

bar_plot_demo1 <-subset_offers |>ggplot(aes(x = Demo1, fill = Status)) +
  coord_flip() +
  geom_bar(position = "fill") +theme_bw()
bar_plot_demo2 <-offers |>ggplot(aes(x = Demo2, fill = Status)) +coord_flip() +
  geom_bar(position = "fill") +theme_bw()
bar_plot_demo3 <-ggplot(offers,aes(x = Demo3, fill = Status)) +coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  geom_bar(position = "fill") +theme_bw()

combined_bar_plots <- cowplot::plot_grid(bar_plot_demo1, bar_plot_demo2,
                                         bar_plot_demo3, ncol = 2,rel_heights = c(1, 2))

#Bar Plots Illustrating each of the student application prospects variables
#and their impact on enrollment status
bar_plot_App1 <-subset_offers |>ggplot(aes(x = App1, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +theme_bw()
bar_plot_App2 <-offers |>ggplot(aes(x = App2, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +theme_bw()
bar_plot_App3 <-offers |>ggplot(aes(x = App3, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +theme_bw()
bar_plot_App4 <-offers |>ggplot(aes(x = App4, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +theme_bw()
combined_bar_plotsapp <- cowplot::plot_grid(bar_plot_App1, bar_plot_App3,
                                            bar_plot_App2,bar_plot_App4, ncol = 2)

#Bar Plots Illustrating each of the students' educational background
#and its impact on enrollment status

bar_plot_Edu1 <-subset_offers |>ggplot(aes(x = Edu1, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +theme_bw()
bar_plot_Edu2 <-subset_offers |>ggplot(aes(x = Edu2, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +theme_bw()
bar_plot_Edu3 <-subset_offers |>ggplot(aes(x = Edu3, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +theme_bw()
combined_bar_plotsedu <- cowplot::plot_grid(bar_plot_Edu1, bar_plot_Edu2,
                                            bar_plot_Edu3, ncol = 2)







