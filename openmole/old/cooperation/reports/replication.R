library(tidyverse)

repdata <- read_csv("../results/lhs.csv")

rescuedDynamics <- select(repdata, starts_with("rescuedDynamic")) %>%
  mutate(index = row_number()) %>%
  gather("timestep", "rescued", -index) %>%
  mutate(timestep=as.numeric(str_remove(timestep, "rescuedDynamic"))) %>%
  group_by(index) %>%
  mutate(rescued = cumsum(rescued))

ggplot(rescuedDynamics) +
  geom_line(aes(x=timestep, y=rescued, group=index, colour=as.factor(index)), alpha=0.5, show.legend=FALSE)
  
