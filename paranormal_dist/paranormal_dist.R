library(tidyverse)
library(extrafont)
library(patchwork)

g1 <- data.frame(rango = seq(-3, 3, by = 0.01)) %>%
  mutate(y = dnorm(rango)) %>%
  ggplot(aes(x = rango,
           y = y)) + geom_line(size = 1.1) + theme_classic(base_size = 16) +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(x = "", y = "") +
  annotate("text", x = 0, y = 0.5, label = "NORMAL DISTRIBUTION", family = "Kristen ITC", size = 8)

seno <- sin(seq(-pi/2, pi*7.5, by = 0.125))
t_st <- dt(seq(-1, 1, by = 0.01), 1)
y2 <- (seno + 1)/100 + min(t_st)

g2 <- data.frame(rango = seq(-1, 1, by = 0.01)) %>%
  mutate(y = dt(rango, 1),
         y2 = y2[-1]) %>%
  ggplot(aes(x = rango,
             y = y)) + geom_line(size = 1.1) + 
  geom_line(aes(x = rango, y = y2), size = 1.1) + 
  geom_point(data = data.frame(x = c(-0.15, 0.05), y = rep(0.27, 2)),
             mapping = aes(x = x, y = y), size = 4) +
  theme_classic(base_size = 16) +
  scale_y_continuous(limits = c(0.1, 0.4)) + scale_x_continuous(limits = c(-3, 3)) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(x = "", y = "") +
  annotate("text", x = 0, y = 0.375, label = "PARANORMAL DISTRIBUTION", family = "Kristen ITC", size = 8)

g1/g2 + 
  patchwork::plot_annotation(caption = "Idea original: Freeman, M. (2006). A visual comparison of normal and paranormal distributions.\nJournal of Epidemiology and Community Health, 60(1), 6.")

ggsave("paranormal_distribution.png", dpi = 300, width = 6.3, height = 8)
