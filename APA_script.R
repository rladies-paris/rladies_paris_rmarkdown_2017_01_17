#### Data Cleaning ####

anscombe_summary = gather(anscombe)

data_bar = anscombe_summary[45:88,] %>%
  group_by(key) %>%
  summarise(mean = mean(value),
            sd = sd(value)) %>%
  ungroup() %>%
  mutate(sd_min = mean - sd) %>%
  mutate(sd_max = mean + sd)


data_descriptives = data_bar %>%
  select(key, mean, sd)



#### Figures for the APA Example paper ####

colors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# I am using color-blind friendly colors, so I need to either specify the respective palette or for maximal control manually define them. 


data.barplot = ggplot(data_bar, aes(x = key, y = mean)) +
  geom_bar(stat = "identity", aes(fill = key, alpha = .5)) +
  geom_errorbar(aes(ymin = sd_min, ymax = sd_max), width = 0.25, lwd=1) +
  coord_cartesian(ylim=c(0,16), expand = FALSE) +
  scale_fill_manual(values = c(colors[[2]], colors[[3]], colors[[4]], colors[[5]])) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(),
        text=element_text(size=24), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(), axis.text.x = element_blank())


### The Boxplot

data.box = ggplot(anscombe_summary[45:88,], aes(x = key, y = value)) +
  geom_boxplot(aes(fill = key, alpha = .5)) +
  scale_y_continuous(limits = c(0, 16), expand = c(0,0)) +
  scale_fill_manual(values = c(colors[[2]], colors[[3]], colors[[4]], colors[[5]])) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(),
        text=element_text(size=24), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(), axis.text.x = element_blank())


### The Quartet

p1 <- ggplot(anscombe) + 
  geom_point(aes(x1, y1), color = colors[[2]], size = 3) + 
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(2, 20), expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(2, 14, 2), limits = c(2, 14), expand = c(0,0)) + 
  geom_abline(intercept = 3, slope = 0.5, color = colors[[1]]) + 
  expand_limits(x = 0, y = 0) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=24), title=element_text(size=24), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(), axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(), legend.title = element_blank(), strip.background = element_rect(color="white", fill="white"))

p2 <- ggplot(anscombe) + geom_point(aes(x2, y2), color = colors[[3]], size = 3) + 
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(2, 20), expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(2, 14, 2), limits = c(2, 14), expand = c(0,0)) + 
  geom_abline(intercept = 3, slope = 0.5, color = colors[[1]]) + 
  expand_limits(x = 0, y = 0) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=24), title=element_text(size=24), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(), axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(), legend.title = element_blank(), strip.background = element_rect(color="white", fill="white"))

p3 <- ggplot(anscombe) + geom_point(aes(x3, y3), color = colors[[4]], size = 3) + 
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(2, 20), expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(2, 14, 2), limits = c(2, 14), expand = c(0,0)) + 
  geom_abline(intercept = 3, slope = 0.5, color = colors[[1]]) + 
  expand_limits(x = 0, y = 0) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=24), title=element_text(size=24), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(), axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(), legend.title = element_blank(), strip.background = element_rect(color="white", fill="white"))

p4 <- ggplot(anscombe) + geom_point(aes(x4, y4), color = colors[[5]], size = 3)  + 
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(2, 20), expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(2, 14, 2), limits = c(2, 14), expand = c(0,0)) + 
  geom_abline(intercept = 3, slope = 0.5, color = colors[[1]]) + 
  expand_limits(x = 0, y = 0) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=24), title=element_text(size=24), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(), axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(), legend.title = element_blank(), strip.background = element_rect(color="white", fill="white"))
