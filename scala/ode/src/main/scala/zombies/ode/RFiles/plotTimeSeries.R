library(tidyverse)

# Real data
file = read_csv("ZombielandData_1000repli.csv") %>%
    select(contains("Avg"))

# ODE data
simul0 <- read_csv("simulODE.csv")
simul <- read_csv("simulODE_calib_190522.csv")
# simulOut <- read_csv("simulODE_out.csv")
# simulFightback <- read_csv("simulODE_fightback.csv")
# simulDie <- read_csv("simulODE_die.csv")

prepareSimulData <- function(dataset, mecanism) {
    res <- dataset %>%
        mutate(time_step = seq(1, nrow(.))) %>%
        gather(contains("ing"), key = "category", value = "nb") %>%
        mutate(species = ifelse(grepl("humans", category, ignore.case = T), "humans", "zombified"),
               originData = mecanism,
               speed = ifelse(grepl("walk", category, ignore.case = T), "walking", "running")) %>%
        select(originData, species, speed, time_step, nb)

    return(res)
}

allData <- prepareSimulData(file, "ABM") %>%
    full_join(prepareSimulData(simul0, "no tWarp")) %>%
    full_join(prepareSimulData(simul, "tWarp")) %>%
    full_join(prepareSimulData(simulOut, "humans leave")) %>%
    full_join(prepareSimulData(simulFightback, "humans fight back infection")) %>%
    full_join(prepareSimulData(simulDie, "humans kill zombies"))

allData$originData <- fct_relevel(allData$originData, "ABM", "none", "humans leave", "humans fight back infection")

plot_dynamics <- allData %>%
    ggplot(aes(x = time_step, y = nb, color = originData)) +
    geom_line() +
    xlab("time step") +
    scale_color_discrete(name = "Mechanisms") +
    facet_grid(speed ~ species) +
    theme_bw()
plot_dynamics

ggsave("~/Documents/OpenMOLE/ExModelo/courses/ODE/presentation/figures/ABMdynamics.png", plot_dynamics, width = 15, units = "cm")


source("fitness.R")
LL <- logLik(simul0$humansWalking, file$walkingHumansAvg) +
    logLik(simul0$humansRunning, file$runningHumansAvg) +
    logLik(simul0$zombifiedWalking, file$walkingZombiesAvg) +
    logLik(simul0$zombifiedRunning, file$runningZombiesAvg)

print(LL)



sommes <- plot_data %>%
    spread(key = speed, value = nb) %>%
    group_by(originData, species) %>%
    mutate(total = walking + running)

plot_dynamics_sum <- sommes %>%
    filter(originData != "ODE_OMS") %>%
    ggplot(aes(x = time, y = total, color = originData)) +
    geom_line() +
    # scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
    xlab("time step") +
    facet_wrap(~ species, nrow = 2) +
    theme_bw()
plot_dynamics_sum
