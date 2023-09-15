#### Assignment 3 ####

rm(list = ls())  # clear all variables in workspace
graphics.off()  # clear all plots


library(ggplot2)
library(lme4)
library(glmmTMB)
library(nlme)
library(car)
library(tidyverse) #for all data wrangling
library(cowplot) #for manuscript ready figures
library(effects)
library(sjPlot)

library(reshape2)  # data wrangling
library(lattice)   # for plotting
library(sjPlot)    # to visualizing random effects
library(ggeffects) # for plotting predictions of MEM
library(knitr)     # beautifying tables

library(report)    # to report the model results
#library(lsmeans)   # for a post-hoc analysis
library(broom)     # for tidy results



# Set working directory
setwd("C:/Users/Magnus/Desktop/DTU/02424 Advanced Dataanalysis and statistical modelling/Project 3")





data <- read.table("clothingFullAss03.csv", sep=",",header=TRUE)




#################### 1. Data Presentation ####################
# Define the color palette
colors <- c("female" = "#FF9999", "male" = "#56B4E9")

# sex
ggplot(data, aes(x = sex, y = clo, fill = sex)) +
  scale_fill_manual(values = colors) + # Change color of borders
  geom_boxplot() +
  labs(title = "Boxplot of clo by sex", x = "sex", y = "clo") +
  theme_bw()+
  theme(text = element_text(size = 20)) # Increase legend title size

# subjId
ggplot(data, aes(x = factor(subjId), y = clo, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  labs(title = "Boxplot of clo by subjId", x = "subjId", y = "clo") +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 12)
        )



# Create the plot
ggplot(data, aes(x = time2, y = clo, group = interaction(subjId,day))) +
  geom_line(aes(colour = sex), linetype=1)+
  scale_color_manual(values = colors) +
  labs(x = "Day", y = "Clo", color = "Sex") +
  theme_bw()+
  theme(text = element_text(size = 20)) # Increase legend title size


# Within day
day1 <- data[data$day == 1,]
day2 <- data[data$day == 2,]
day3 <- data[data$day == 3,]
day4 <- data[data$day == 4,]

# day 1
ggplot(day1, aes(x = time2, y = clo, group = interaction(subjId,subDay))) +
  geom_line(aes(colour = sex),size=2,linetype=1)+
  scale_color_manual(values = colors) +
  labs(x = "Observation number", y = "Clo", color = "Sex") +
  theme_bw()+
  theme(text = element_text(size = 22)) # Increase legend title size

# day 2
ggplot(day2, aes(x = time2, y = clo, group = interaction(subjId,subDay))) +
  geom_line(aes(colour = sex), linetype=1,size=2)+
  scale_color_manual(values = colors) +
  labs(x = "Observation number", y = "Clo", color = "Sex") +
  theme_bw()+
  theme(text = element_text(size = 22)) # Increase legend title size

# day 3
ggplot(day3, aes(x = time2, y = clo, group = interaction(subjId,subDay))) +
  geom_line(aes(colour = sex), linetype=1,size=2)+
  scale_color_manual(values = colors) +
  labs(x = "Observation number", y = "Clo", color = "Sex") +
  theme_bw()+
  theme(text = element_text(size = 22)) # Increase legend title size

# day 4
ggplot(day4, aes(x = time2, y = clo, group = interaction(subjId,subDay))) +
  geom_line(aes(colour = sex), linetype=1,size=2)+
  scale_color_manual(values = colors) +
  labs(x = "Observation number", y = "Clo", color = "Sex") +
  theme_bw()+
  theme(text = element_text(size = 22)) # Increase legend title size




#################### 2. Mixed effect model with subjId as random effect #################### 
# maybe consider non-normal distribution 


mixed <- lmer(clo ~ as.factor(day) + time + sex + tInOp + tOut + sex:tOut + sex:tInOp + (tInOp+tOut|subjId), data=data, REML = 0)
#### Convergence problems..... ask TA
mixed <-lmer(clo ~ as.factor(day) + time + sex + tInOp + tOut + sex:tOut + sex:tInOp + (tInOp+tOut|subjId), data=data, REML=FALSE, control = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
summary(mixed)


mixed_tOut <- lmer(clo ~ as.factor(day) + time + sex + tInOp + tOut + sex:tOut + sex:tInOp + (tOut|subjId), data=data, REML=FALSE, control = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
mixed_tInOp <- lmer(clo ~ as.factor(day) + time + sex + tInOp + tOut + sex:tOut + sex:tInOp + (tInOp|subjId), data=data, REML=FALSE, control = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))

# Test if we should keep the random slopes:
anova(mixed, mixed_tOut) # highly significant
anova(mixed, mixed_tInOp) # highly significant


# Model reduction using nested submodel and comparing using type III ANOVA table
Anova(mixed, type="III")
mixed1 <- update(mixed, ~. -sex:tInOp, REML=FALSE)

Anova(mixed1, type="III")
mixed3 <- update(mixed1, ~. -tInOp, REML=FALSE)

Anova(mixed3, type="III")


# refit using REML
mixed3 <- update(mixed3, ~., REML=TRUE)
summary(mixed3)
confint(mixed3)

# The variation among levels of the random effects can be shown by plotting the departure from the overall model estimate for intercepts and slopes:
dotplot(ranef(mixed3, condVar=T))


# maybe not include
plot_model(mixed3, type = "resid", show.values = FALSE) + theme_bw()

# residuals
set_theme(
  base = theme_bw(),
  axis.tickslen = 0, # hides tick marks
  axis.title.size = 1.5,
  axis.textsize = 1.2,
  legend.size = 1,
  legend.title.size = .8,
  geom.label.size = 3.5
)
plot_model(mixed3, type = "diag") 
















#### 3. Fit a mixed effect model that include subjId and day (nested to subjId) as random effects ####
# approximate with subDay


# m3 <-lmer(clo ~ sex + tOut + tInOp + tOut:sex + tInOp:sex + (tInOp+tOut|subjId)
#                 + (tInOp+tOut|subDay),
#                 data=data, REML=FALSE,
#                 control = lmerControl(optimizer = "nloptwrap",
#                 calc.derivs = FALSE))


# What I would assume as the right model
m3.0 <- lmer(clo ~ sex + tOut + tInOp + tOut:sex + tInOp:sex + (tInOp+tOut|subjId/day),
           data = data, REML=FALSE, 
           control = lmerControl(optimizer = "nloptwrap",
                                 calc.derivs = FALSE))
summary(m3.0)



# The approximation of the above model (using subDay)
m3 <- lmer(clo ~ as.factor(day) + time + sex + tOut + tInOp + tOut:sex + tInOp:sex + (tInOp+tOut|subDay),
           data = data, REML=FALSE, 
           control = lmerControl(optimizer = "nloptwrap",
                                 calc.derivs = FALSE))
summary(m3)
# Ask about m3.0 being singular, what to do, and if this modelk is ok instead
# We go with it for now

# consider random effects
m3a <- lmer(clo ~ as.factor(day) + time + sex + tOut + tInOp + tOut:sex + tInOp:sex + (tOut|subDay),
            data = data, REML=FALSE, 
            control = lmerControl(optimizer = "nloptwrap",
                                  calc.derivs = FALSE))

m3b <- lmer(clo ~ as.factor(day) + time + sex + tOut + tInOp + tOut:sex + tInOp:sex + (tInOp|subDay),
            data = data, REML=FALSE, 
            control = lmerControl(optimizer = "nloptwrap",
                                  calc.derivs = FALSE))


anova(m3, m3a) # significant
anova(m3, m3b) # significant




Anova(m3, type="III")
# Drop sex:tOut
m3.1 <- update(m3, ~. -sex:tOut)


Anova(m3.1, type="III")
m3.2 <- update(m3.1, ~. -time)

Anova(m3.2, type="III")
m3.3 <- update(m3.2, ~. -as.factor(day))

Anova(m3.3, type="III")
m3.4 <- update(m3.3, ~. -tOut)

Anova(m3.4, type="III")
m3.5 <- update(m3.4, REML=TRUE)

summary(m3.5)

# Plots and residuals
# The variation among levels of the random effects can be shown by plotting the departure from the overall model estimate for intercepts and slopes:
dotplot(ranef(m3.5, condVar=T))

set_theme(
  base = theme_bw(),
  axis.tickslen = 0, # hides tick marks
  axis.title.size = 1.5,
  axis.textsize = 1.2,
  legend.size = 1,
  legend.title.size = .8,
  geom.label.size = 3.5
)
plot_model(m3.5, type = "diag") 










#### 4. Fit a model including within day auto-correlation (repeated measurement set up),
# with subDay as random effect, you should only consider random intercepts in these models. ####

m4 <- lme(clo ~ (sex + tOut + tInOp)^2, random= ~1|subDay, data = data, correlation = corAR1(form=~as.numeric(time)|subDay), method="ML")
m4.exp <- lme(clo ~ (sex + tOut + tInOp)^2, random= ~1|subDay, data = data, correlation = corExp(form=~as.numeric(time)|subDay), method="ML")

## AR 1 ¤¤
Anova(m4, type="III")
m4.1 <- update(m4, ~. -sex:tOut)
Anova(m4.1, type="III")

m4.2 <- update(m4.1, ~. -tOut:tInOp)
Anova(m4.2, type="III")


## Exp ##
Anova(m4.exp, type="III")
m4.exp1 <- update(m4.exp, ~. -sex:tOut)
Anova(m4.exp1, type="III")

m4.exp2 <- update(m4.exp1, ~. -tOut:tInOp)
Anova(m4.exp2, type="III")

m4.exp3 <- update(m4.exp2, ~. -tOut)
Anova(m4.exp3, type="III")



summary(m4.2)
summary(m4.exp3)

# update with REML
m4.exp3 <- update(m4.exp3, method = "REML")
summary(m4.exp3)
intervals(m4.exp3)


colors <- c("female" = "#FF9999", "male" = "#56B4E9")
plot_model(m4.exp3, type = "diag") 
plot(Variogram(m4.exp3), main='Exponential')

dataplot <- ggplot(data, aes(x = tInOp, y = clo, group = interaction(subjId,day))) +
  geom_point(aes(colour = sex))+
  scale_color_manual(values = colors) +
  labs(x = "tInOp", y = "Clo", color = "Sex") +
  theme_bw()+
  theme(text = element_text(size = 20)) # Increase legend title size
dataplot


plot1 <- sjPlot::plot_model(m4.exp3, type = "pred", terms = c("tInOp", "sex"))
plot1 + geom_point(data = data, aes(x = tInOp, y = clo, colour=sex), inherit.aes = FALSE) +
        scale_color_manual(values = colors) +
        labs(x = "tInOp", y = "Clo", color = "Sex") +
        theme_bw()+xlim(22.5,32.5)
        theme(text = element_text(size = 20)) # Increase legend title size



sjPlot::plot_model(m4.exp3, type="pred", terms=c("sex","clo"))







coef <- coefficients(m4.exp3)


plot(data$clo[data$subjId==0] ~data$tInOp[data$subjId==0], col=data$day, ylim=c(0,1), xlim=c(22,30),
     ylab=('clo'), xlab=('tInOp'))
title('Visualization of the random intercepts for subjId=0')
legend("topright", legend=c("Day 1", "Day 2", "Day 3"),
       col=c(1,2,3), lty=1)

for (i in (1+0):(1+2)) {
  abline(a=coef[i,1], 
         b=coef[i,3], col=i)  
}

j <- 2
plot(data$clo[data$subjId==j] ~data$tInOp[data$subjId==j], col=data$day, ylim=c(0,1), xlim=c(22,30),
     ylab=('clo'), xlab=('tInOp'))
title('Visualization of the random intercepts for subjId=2')
legend("bottomright", legend=c("Day 1", "Day 2", "Day 3"),
       col=c(1,2,3), lty=1)

for (i in (1+(j*3-1)+1):(2+(j*3+1))) {
  print(i)
  abline(a=coef[i,1], 
         b=coef[i,3], col=i-(j*3-1)-1)  
  print(i-(j*3-1)-1)
}


#################### Hierarchical models: Random variance #################### 
rm(list = ls())  # clear all variables in workspace
graphics.off()  # clear all plots

library(lme4)
library(glmmTMB)
library(nlme)
library(mvnfast)
library(mvtnorm)

# Set working directory
setwd("C:/Users/Magnus/Desktop/DTU/02424 Advanced Dataanalysis and statistical modelling/Project 3")
data <- read.table("clothingFullAss03.csv", sep=",",header=TRUE)
data$subjId = as.factor(data$subjId)
data$day = as.factor(data$day)
data$subDay = as.factor(data$subDay)


#### 1. ####
fit0 <- lmer(clo~sex+(1|subjId),data=data,REML=FALSE)

# design matrix
X <- model.matrix(fit0)
N <- Dim(X)[1]

# Make psi
Psi <- function(sigma_u){
  K_list <- list()
  for (i in unique(data$subjId)){  
    i <- as.numeric(i)
    J <- length(as.numeric(data$day[data$subjId == i]))
    count <- 0
    for (j in unique(as.numeric(data$day[data$subjId == i]))){
      n_measurements <- length(data$time2[data$subjId == i & data$day == j])
      count <- count + n_measurements
    }
    
    K_list[[i+1]] <- matrix(sigma_u, nrow=count, ncol=count)
  }
  Psi <- as.matrix(bdiag(K_list))
  return(Psi)
}

# negative log likelihood
logLik <- function(theta){
  Psi <- Psi(theta[4]^2)
  Sigma <- theta[3]^2 * diag(N)
  V <- Sigma + Psi
  beta <- c(theta[1], theta[2])
  -dmvnorm(data$clo, as.vector(X %*% beta), V, log=TRUE)
  #-1/2*log(determinant(V)$modulus[1] - 1/2*t(data$clo - X%*%beta)%*%solve(V)%*%(data$clo-X%*%beta))
}

opt <- nlminb(c(mean(data$clo), mean(data$clo), 0.1, 0.1), logLik,
             lower = c(-Inf, -Inf, 1e-10, 1e-10)) # it minimizes negative log likelihood

c(opt$par[1], opt$par[2], opt$par[3]^2, opt$par[4]^2) 
c(0.59176, -0.08322, 0.009768, 0.013608) # real parameters





#### 2. #### 
fit1 <- lmer(clo~sex+(1|subjId)+(1|subjId:day),data=data,REML=FALSE)

# design matrix
X <- model.matrix(fit1)
N <- Dim(X)[1]


Phi <- function(sigma_v){
  K_list <- list()
  count <- 1
  for (i in unique(data$subjId)){  
    i <- as.numeric(i)
    
    for (j in unique(as.numeric(data$day[data$subjId == i]))){
      n_measurements <- length(data$time2[data$subjId == i & data$day == j])
      K_list[[count]] <- matrix(sigma_v, n_measurements, n_measurements)
      count <- count +1
    }
  }
  Phi = as.matrix(bdiag(K_list))
  return(Phi)
}

# negative log likelihood
logLik2 <- function(theta){
  Sigma <- theta[3]^2 * diag(N)
  Psi <- makePsi(theta[4]^2)
  Phi <- Phi(theta[5]^2)
  V <- Sigma + Psi + Phi
  beta <- c(theta[1], theta[2])
  -dmvnorm(data$clo, as.vector(X %*% beta), V, log=TRUE)
}

opt <- nlminb(c(mean(data$clo), mean(data$clo), 0.1, 0.1, 0.1), logLik2,
             lower = c(-Inf, -Inf, 1e-10, 1e-10)) # it minimizes negative log likelihood

c(opt$par[1], opt$par[2], opt$par[3]^2, opt$par[4]^2, opt$par[5]^2) 
c(0.59242, -0.08439, 0.003132641, 0.0108056, 0.009467290)




#### 3 ####
Psi <- function(sigma_u, alpha_male, alpha_female){
  K_list <- list()
  for (i in unique(data$subjId)){  
    i <- as.numeric(i)
    J <- length(as.numeric(data$day[data$subjId == i]))
    count <- 0
    
    # scaling of variance depending on sex
    if (unique(data$sex[data$subjId == i])  == 'male'){  
      sigma_i <- sigma_u * alpha_male
    } else {
      sigma_i <- sigma_u * alpha_female
    }
    
    for (j in unique(as.numeric(data$day[data$subjId == i]))){
      n_measurements <- length(data$time2[data$subjId == i & data$day == j])
      count <- count + n_measurements
    }
    
    K_list[[i+1]] <- matrix(sigma_i, nrow=count, ncol=count)
  }
  Psi <- as.matrix(bdiag(K_list))
  return(Psi)
}

Phi <- function(sigma_v, alpha_male, alpha_female){
  K_list = list()
  count = 1
  for (i in unique(data$subjId)){  
    i = as.numeric(i)
    # scaling of variance depending on sex
    if (unique(data$sex[data$subjId == i])  == 'male'){  
      sigma_i <- sigma_v * alpha_male
    } else {
      sigma_i = sigma_v * alpha_female
    }
    for (j in unique(as.numeric(data$day[data$subjId == i]))){
      n_measurements <- length(data$time2[data$subjId == i & data$day == j])
      K_list[[count]] <- matrix(sigma_i, n_measurements, n_measurements)
      count <- count +1
    }
  }
  Phi <- as.matrix(bdiag(K_list))
  return(Phi)
}

Sigma <- function(sigma, alpha_male, alpha_female){
  Sigma_list <- list()
  count <- 1
  for (i in unique(data$subjId)){
    obs_i <- length(data$subjId[data$subjId == i]) 
    if (unique(data$sex[data$subjId == i]) == 'male'){
      sigma_i <- sigma * alpha_male
    }else{
      sigma_i <- sigma * alpha_female
    }
    Sigma_list[[count]] <- diag(sigma_i, obs_i, obs_i)
    count <- count + 1
  }
  return(as.matrix(bdiag(Sigma_list)))
}




# negative log likelihood
logLik3 <- function(theta){
  Sigma_mat <- Sigma(theta[3]^2, theta[6], theta[7])
  Psi <- Psi(theta[4]^2, theta[6], theta[7])
  Phi <- Phi(theta[5]^2, theta[6], theta[7])
  beta <- c(theta[1], theta[2])
  -dmvnorm(data$clo, as.vector(X %*% beta), Sigma_mat + Psi + Phi, log=TRUE)
}


opt <- nlminb(c(mean(data$clo), mean(data$clo), 0.1, 0.1, 0.1, 0.1, 0.1), logLik3,
              lower = c(-Inf, -Inf, 1e-10, 1e-10, 1e-10, 1e-10)) # it minimizes negative log likelihood

c(opt$par[1], opt$par[2], opt$par[3]^2, opt$par[4]^2, opt$par[5]^2, opt$par[6], opt$par[7])




#### 5 ####
Psi <- function(sigma_u, alpha_male, alpha_female){
  K_list <- list()
  for (i in unique(data$subjId)){  
    i <- as.numeric(i)
    J <- length(as.numeric(data$day[data$subjId == i]))
    count <- 0
    
    # scaling of variance depending on sex
    if (unique(data$sex[data$subjId == i])  == 'male'){  
      sigma_i <- sigma_u * alpha_male
    } else {
      sigma_i <- sigma_u * alpha_female
    }
    
    for (j in unique(as.numeric(data$day[data$subjId == i]))){
      n_measurements <- length(data$time2[data$subjId == i & data$day == j])
      count <- count + n_measurements
    }
    K_list[[i+1]] <- matrix(sigma_i, nrow=count, ncol=count)
  }
  return(as.matrix(K_list))
}

Phi <- function(sigma_v, alpha_male, alpha_female){
  Phi_list <- list()
  for (i in unique(data$subjId)){  
    i <- as.numeric(i)
    Phi_i <- list()
    
    if (unique(data$sex[data$subjId == i])  == 'male'){  #
      sigma_i <- sigma_v * alpha_male
    } else {
      sigma_i <- sigma_v * alpha_female
    }
    
    for (j in unique(as.numeric(data$day[data$subjId == i]))){
      n_measurements <- length(data$time2[data$subjId == i & data$day == j])
      Phi_i[[j]] <- matrix(sigma_i, n_measurements, n_measurements)
    }
    Phi_list[[i+1]] <- as.matrix(bdiag(Phi_i))
  }
  return(Phi_list)
}

Sigma <- function(sigma, alpha_male, alpha_female){
  Sigma_list <- list()
  for (i in unique(data$subjId)){
    obs_i <- length(data$subjId[data$subjId == i]) # observations for subjId i
    
    if (unique(data$sex[data$subjId == i]) == 'male'){
      sigma_i <- sigma * alpha_male
    }else{
      sigma_i <- sigma * alpha_female
    }
    Sigma_list[[count]] <- diag(sigma_i, obs_i, obs_i)
  }
  return(as.matrix(Sigma_list))
}


# Maximum Likelihood
logLik5 <- function(theta){  
  Sigma_mat <- Sigma(theta[3]^2, theta[6], theta[7])
  Psi_mat <- Psi(theta[4]^2, theta[6], theta[7])
  Phi_mat <- Phi(theta[5]^2, theta[6], theta[7])
  beta <- c(theta[1], theta[2])
  
  logL <- 0
  for (i in unique(data$subjId)){
    id <- data$subjId==i
    idx <- as.numeric(i) + 1
    logL <- logL + dmvt(data$clo[id], as.vector(X[id, ] %*% beta),
                       (Sigma_mat[[idx]] + Psi_mat[[idx]] + Phi_mat[[idx]])/theta[8],
                       df = 2*theta[8], log=TRUE)
  }
  return(-logL) # return negative log-likelihood
}

theta_init <- c(mean(data$clo),-0.08, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
opt <- nlminb(theta_init, logLik5, lower = c(-Inf, -Inf, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10))
opt$objective
opt$par
c(opt$par[1], opt$par[2], opt$par[3]^2, opt$par[4]^2, opt$par[5]^2, opt$par[6], opt$par[7], opt$par[8])







#### 6 ####
library(tmbstan);library(TMB);
Sys.setenv(PATH = paste("C:/rtools40/usr/bin", Sys.getenv("PATH"), sep = ";"))
Sys.setenv(PATH = paste("C:/rtools40/mingw64/bin", Sys.getenv("PATH"), sep = ";"))
Sys.setenv(BINPREF = "C:/rtools40/mingw64/bin/")

# Compile the TMB model
compile("new_mixed_model.cpp")
dyn.load(dynlib("new_mixed_model"))

data$sex_numeric <- as.integer(data$sex == "male")
data_tmb <- list(clo = data$clo,
                 sex = data$sex_numeric,
                 subjId = as.integer(data$subjId) - 1)

obj <- MakeADFun(data = data_tmb,
                 parameters = list(beta = 0,
                                   log_sigma = 0,
                                   log_sigma_u = 0,
                                   log_sigma_v = 0,
                                   log_sigma_G = 0,
                                   u = rep(0, nrow(data)),
                                   v = rep(0, nrow(data)),
                                   gamma = rep(0, nrow(data))),
                 random = c("u", "v", "gamma"),
                 silent = TRUE)

# Optimize the model
opt <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)

# Extract the results
report <- sdreport(obj)

# Print the results
print(report)

# Calculate standard errors
std_errors <- sqrt(diag(vcov(report)))
print(std_errors)















