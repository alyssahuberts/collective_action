################
# Script to conduct power calculations for conjoint (CIA)
# Date created: July 15, 2021
# Date last update July 15,2021
################
library(tidyverse)
library(lfe)
library(gridExtra)

#############################
# Format Mock Data (actually this is totally unnecessary but did help me think
# about the structure of the data we'll need)
#############################
set.seed(112521)


# The data will vary on five dimensions:
  # problem 1 (shootings/water)
  # Neighbors' response (information/private/govt) Do we want to add did nothing?
  # Results 1 (Social capital, formal organization)
  # Results 2 (Problem improved/Problem did not improve)
  # Second issue (shootings/water)

# Then we'll have outcomes (choice, forced choice, or rating)

# Create a mock data set with 3500 respondents x 5 choices. (We'll look at how
# many pairs they actually need to see later down)
mock <- tibble(p1=sample(c("shootings", "water"),17500, replace = TRUE),
               a = sample(c("information", "private", "gov", "nothing"),17500, replace = TRUE),
               r1= sample(c("social", "formal"),17500, replace = TRUE),
               r2 = sample(c("success", "failure"), 17500, replace = TRUE),
               p2 = sample(c("shootings", "water"),17500, replace=TRUE),
               id= rep(1:3500, 5))


#######################
# Power Calc 
#######################
# This first example varies the sample size. In our case, this is the number of profiles that each individual will see. 
# This one focuses on forced choice, with a control mean assumed of .5 and sd of
# .25 (not sure if we can find a more realistic one without a pilot)

possible.ns <- seq(from=100, to=5000, by=50)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N
cl_size <- 5                                     # cluster size- note that our clusters will be the individual id 

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=.5, sd=.25)              # control potential outcome
    tau <- .1                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters for individuals)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))

possible.ns[24]
powers[24]

##### Power by effect size #####

possible.es <- seq(from=0, to=.5, by=.025)     # The possible effect sizes we'll be considering
powers <- rep(NA, length(possible.es))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N
cl_size <- 5                                     # cluster size
N <- 3500*5                                         # Number of respondents

#### Outer loop to vary the effect size ####
for (j in 1:length(possible.es)){
  tau <- possible.es[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=.3, sd=.47)              # control potential outcome
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
ggplot() +
  geom_point(aes(x= possible.es, y = powers)) +
  theme_bw() +
  labs(x= "Estimated Effect Size (AMCE)",y = "Power", title = "Power Simulations for Vignette 1 \n With 3500 respondents and five profiles") 



