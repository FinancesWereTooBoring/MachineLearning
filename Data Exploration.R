#####
subset_offers <- subset(offers, AppYear != 2023)


# bar plot illustrating the class imbalance
#in the target variable 
imbalance_class <- ggplot(subset_offers, aes(x = Status, fill = Status)) +
  geom_bar() +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw() +
  coord_flip()+
  labs(title = "Class Imbalance in Status",
       x = "Status",
       y = "Count")
#ggsave(filename = "imbalance", imbalance_class, path = "data", device = "png", width = 10.1, height = 6.76)



#bar plot illustrating the Responses of Enrolled 
#and Non-enrolled Individuals
responses <- ggplot(subset_offers, aes(x = Response, fill = Status)) +
  geom_bar() +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw() +
  labs(title="Responses by Enrollement Status", x = "Response",
                   y = "Count")
#ggsave(filename = "responses", responses, path = "data", device = "png", width = 10.1, height = 6.76)


#Bar Plot illustrating the First Source of Information about the
#University among Enrolled and Non-enrolled Individuals

first_heard <- ggplot(subset_offers, aes(x = HowFirstHeard, fill = Status)) +
  geom_bar(position = "fill") +
  theme_bw() +scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs( title="Enrollement Status based on 'How did you first heard about RSM'",
        x = "HowFirstHeard", y = "Proportion")

ggsave(filename = "first_heard", first_heard, path = "data", device = "png", width = 10.1, height = 6.76)



#bar plots depicting each demographic variable
#and their Influence on Enrollment Status

bar_plot_demo1 <-subset_offers |>ggplot(aes(x = Demo1, fill = Status)) +
  coord_flip() +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()

bar_plot_demo2 <-subset_offers |>ggplot(aes(x = Demo2, fill = Status)) +coord_flip() +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()

bar_plot_demo3 <-ggplot(subset_offers,aes(x = Demo3, fill = Status)) +coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()

combined_bar_plots <- cowplot::plot_grid(bar_plot_demo1, bar_plot_demo2,
                                         bar_plot_demo3, ncol = 2,rel_heights = c(1, 2))
ggsave(filename = "demo", combined_bar_plots, path = "data", device = "png", width = 10.1, height = 6.76)


library(cowplot)

#Education plots
bar_plot_edu1 <-subset_offers |>ggplot(aes(x = Edu1, fill = Status)) +
  coord_flip() +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()
#ggsave("edu1", plot = bar_plot_demo1, path = "data", device = "jpeg")

bar_plot_edu2 <-subset_offers |>ggplot(aes(x = Edu2, fill = Status)) +coord_flip() +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()
#ggsave("edu2", plot = bar_plot_demo2, path = "data", device = "png")

bar_plot_edu3 <-ggplot(subset_offers,aes(x = Edu3, fill = Status)) +coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()
#ggsave("edu3", plot = bar_plot_demo3, path = "data", device = "png")

combined_bar_plots_edu <- cowplot::plot_grid(bar_plot_edu1, bar_plot_edu2,
                                         bar_plot_edu3, ncol = 2,rel_heights = c(2, 2))
ggsave(filename = "edus", combined_bar_plots_edu, path = "data", device = "png", width = 10.1, height = 8)

#Bar Plots Illustrating each of the student application prospects variables
#and their impact on enrollment status
bar_plot_App1 <-subset_offers |>ggplot(aes(x = App1, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()
bar_plot_App2 <- subset_offers |>ggplot(aes(x = App2, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()
bar_plot_App3 <- subset_offers |>ggplot(aes(x = App3, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()
bar_plot_App4 <- subset_offers |>ggplot(aes(x = App4, fill = Status)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_viridis_d(option = 'E', direction = -1, end = 0.8) +
  theme_bw()

combined_bar_plotsapp <- cowplot::plot_grid(bar_plot_App1, bar_plot_App3,
                                            bar_plot_App2,bar_plot_App4, ncol = 2)
ggsave(filename = "apps", combined_bar_plotsapp, path = "data", device = "png", width = 10.1, height = 6.76)


subset_offers |>ggplot(aes(x = Response, fill = Status)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = .8) +
  theme_bw() +
  facet_wrap(~AppYear)
