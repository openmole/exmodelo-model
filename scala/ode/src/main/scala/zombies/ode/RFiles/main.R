library(deSolve)
library(tidyverse)


# Real data
file = read_csv("ZombielandData_1000repli.csv") %>%
    select(contains("Avg"))
# ODE parameters
# # calib no tWarp
# panic0 = 7.252818
# staminaH = 0.9997521
# inf0 = 0.01977554
# hunt0 = 10.153002
# staminaZ = 1.280096
# # calib tWarp
# panic0 = 100
# staminaH = 0.10866696052269618
# inf = 0.004585325476695915
# hunt0 = 7.974418405424284
# staminaZ = 0.6890335279647442
# read from file
# load("calib_parcimony.RData")
data <- read_csv("~/Documents/OpenMOLE/ExModelo/courses/ODE/profiles/calib.csv")
row = 1
panic0 = as.double(data[row, "panic0"])
staminaH = as.double(data[row, "staminaH"])
hunt0 = as.double(data[row, "hunt0"])
staminaZ = as.double(data[row, "staminaZ"])
inf0 = as.double(data[row, "inf0"])
# out0 = as.double(data[row, "out0"])
# fightback = as.double(data[row, "fightback"])
# die0 = as.double(data[row, "die0"])
# Initial conditions
statesInit = c(250.0, 0.0, 0.0, 4.0)
# Time steps
t0 = 0
dt = 1
tMax = 500
Time = seq(t0, tMax, dt)


## Modele compartimental
source("modele.R")
## Simulation
source("simulation.R")

# Parametres
exhaustH <- 1.0 / staminaH
exhaustZ <- 1.0 / staminaZ

# estim <- c(panic0, exhaustH, hunt0, exhaustZ, inf0, out0, fightback, die0)
estim <- c(panic0, exhaustH, hunt0, exhaustZ, inf0, 0, 0, 0)

simu <- simulation(estim, statesInit, Time, ODE)

## Log-Likelihood calculation for every combination
source("fitness.R")
LL <- logLik(simu$H_walk, file$walkingHumansAvg) +
    logLik(simu$H_run, file$runningHumansAvg) +
    logLik(simu$Z_walk, file$walkingZombiesAvg) +
    logLik(simu$Z_run, file$runningZombiesAvg)

print(LL)

## Visualisation
plot_data <- simu %>%
    bind_cols(file) %>%
    gather(H_walk:walkingZombiesAvg, key = "category", value = "nb") %>%
    mutate(species = ifelse(grepl("H", category), "human", "zombified"),
           originData = ifelse(grepl("Avg", category), "ABM", "ODE"),
           speed = ifelse(grepl("walk", category), "walking", "running"))

plot_dynamics <- plot_data %>%
    ggplot(aes(x = time, y = nb, color = speed, linetype = originData)) +
    geom_line() +
    scale_linetype_manual(values = c("dashed", "solid")) +
    xlab("time step") +
    facet_wrap(~ species, nrow = 2) +
    theme_bw()
plot_dynamics

ggsave("~/Documents/OpenMOLE/ExModelo/courses/ODE/presentation/figures/calib_dynamics.png", plot_dynamics, width = 15, units = "cm")
