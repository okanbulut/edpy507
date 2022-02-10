
# Install the missing packages
install.packages(c("lavaan", "mirt"))

# Activate the required packages
library("DataExplorer")
library("ggcorrplot")
library("psych")
library("lavaan")
library("mirt")
library("ShinyItemAnalysis")

# Set the working directory to wherever you are going to keep the files
setwd("C:/Users/Okan/Desktop/IRT Analysis")

# Read the sapa.csv into R
sapa <- read.csv("sapa_data.csv", header = TRUE)

# Preview the data
head(sapa)

# Structure of the data
str(sapa)

# Check the data
DataExplorer::introduce(sapa)

DataExplorer::plot_intro(sapa)

DataExplorer::plot_missing(sapa)

# Descriptive statistics
psych::describe(x = sapa)

# Correlation matrix of the items
cormat <- psych::tetrachoric(x = sapa)$rho
print(cormat)

# Visualize the correlation matrix
ggcorrplot::ggcorrplot(corr = cormat, # correlation matrix
                       type = "lower", # print only the lower part of the correlation matrix
                       hc.order = TRUE, # hierarchical clustering
                       show.diag = TRUE, # show the diagonal values of 1
                       lab = TRUE, # add correlation values as labels
                       lab_size = 3) # Size of the labels


# Try one-factor EFA model --> nfactors = 1
efa.model1 <- psych::fa(r = sapa, nfactors = 1, fm = "pa", cor = "tet")

# Print the results 
print(efa.model1, sort = TRUE) # Show the factor loadings sorted by absolute value

# Try two-factor EFA model --> nfactors=2
efa.model2 <- psych::fa(sapa, nfactors = 2, rotate = "oblimin", fm = "pa", cor = "tet")

# Print the results 
print(efa.model2, sort = TRUE) # Show the factor loadings sorted by absolute value

# Drop the problematic items
sapa_clean <- subset(sapa, select = -c(rotate.3, rotate.4, rotate.6, rotate.8, matrix.55))

# Try one-factor EFA model with the clean dataset
efa.model3 <- psych::fa(sapa_clean, nfactors = 1, fm = "pa", cor = "tet")
print(efa.model3, sort=TRUE)


# Define a single factor
sapa_model <- 'f =~ reason.4 + reason.16 + reason.17 + reason.19 + letter.7 + 
               letter.33 + letter.34 + letter.58 + matrix.45 + matrix.46 + matrix.47'

# Estimate the CFA model
cfa_sapa <- lavaan::cfa(sapa_model, data = sapa_clean, estimator = "MLR")

# Print the output
summary(cfa_sapa, fit.measures=TRUE, standardized = TRUE)

# Run the item analysis and save it as itemanalysis_psych
itemanalysis_psych <- psych::alpha(x = sapa_clean)

# Print the results
itemanalysis_psych

# Estimate the item parameters for Rasch
model_rasch <- mirt::mirt(data = sapa_clean, # data with only item responses
                          model = 1, # 1 refers to the unidimensional IRT model
                          itemtype = "Rasch", # IRT model we want to use for item calibration
                          verbose = FALSE)

# Extract the item parameters
param_rasch <- coef(model_rasch, # the model object with the estimated parameters
                    IRTpars = TRUE, # whether we want to get traditional IRT parameters
                    simplify = TRUE) # simplify the model output

# What is saved in this object?
str(param_rasch)

# It is a list with a bunch of stuff, but... we only want to keep the item parameters
param_rasch <- as.data.frame(param_rasch$items)

# Print the item parameters
param_rasch


# Define the 1PL model explicitly
model <- "F = 1-11
          CONSTRAIN = (1-11, a1)"

# Estimate the item parameters for 1PL
model_1PL <- mirt::mirt(data = sapa_clean, # data with only item responses
                        model = model, # our specical model for 1PL
                        itemtype = "2PL", # IRT model we want to use for item calibration
                        verbose = FALSE)

# Extract the item parameters
param_1PL <- coef(model_1PL, # the model object with the estimated parameters
                  IRTpars = TRUE, # whether we want to get traditional IRT parameters
                  simplify = TRUE) # simplify the model output

# Only keep the item parameters
param_1PL <- as.data.frame(param_1PL$items)

# Print the item parameters
param_1PL


# Combine the difficulty parameters
b_pars <- data.frame(rasch = param_rasch$b,
                     onePL = param_1PL$b)

# Print the difficulty parameters
b_pars

# Are they correlated?
cor(b_pars$rasch, b_pars$onePL)

# Let's also plot them -- see that they are perfectly aligned on a diagonal line
plot(b_pars$rasch, b_pars$onePL, 
     xlab = "Difficulty (Rasch)", 
     ylab = "Difficulty (1PL)",
     main = "Rasch vs. 1PL Difficulty Parameters")


# Estimate the item parameters for 2PL
model_2PL <- mirt::mirt(data = sapa_clean, # data with only item responses
                        model = 1, # 1 refers to the unidimensional IRT model
                        itemtype = "2PL", # IRT model we want to use for item calibration
                        verbose = FALSE)

# Extract the item parameters
param_2PL <- coef(model_2PL, # the model object with the estimated parameters
                  IRTpars = TRUE, # whether we want to get traditional IRT parameters
                  simplify = TRUE) # simplify the model output

# Only keep the item parameters
param_2PL <- as.data.frame(param_2PL$items)

# Print the item parameters
param_2PL

# Combine the difficulty parameters
b_pars <- data.frame(onePL = param_1PL$b,
                     twoPL = param_2PL$b)

# Print the difficulty parameters
b_pars

# Are they correlated? Yes, they are!
cor(b_pars$onePL, b_pars$twoPL)


# Estimate the item parameters for 3PL
model_3PL <- mirt::mirt(data = sapa_clean, # data with only item responses
                        model = 1, # 1 refers to the unidimensional IRT model
                        itemtype = "3PL", # IRT model we want to use for item calibration
                        verbose = FALSE)

# Extract the item parameters
param_3PL <- coef(model_3PL, # the model object with the estimated parameters
                  IRTpars = TRUE, # whether we want to get traditional IRT parameters
                  simplify = TRUE) # simplify the model output

# Only keep the item parameters
param_3PL <- as.data.frame(param_3PL$items)

# Print the item parameters
param_3PL


# Item 10 - 1PL model
mirt::itemplot(model_1PL, 
               item = 10, # which item to plot
               type = "trace") # traceline (i.e., ICC)


# ICCs for items 1, 3, and 5 in the 2PL model
plot(model_2PL, 
     type = "trace", 
     which.items = c(1, 3, 5)) # items to be plotted (i.e., their positions in the data)


# All ICCs in 3PL model
plot(model_3PL, type = "trace")

# TCC
plot(model_2PL, type = "score")

# IIF
mirt::itemplot(model_2PL, # IRT model that stores the item information
               item = 1, # which item to plot
               type = "infoSE") # plot type


# All IIFs in Rasch model
plot(model_rasch, type = "infotrace")

# TIF and SE for the 3PL model
plot(model_3PL, type = "infoSE")

# Item fit plot for item 1 in the 1PL model
mirt::itemfit(model_1PL, empirical.plot = 1)

# Item fit plot for item 1 in the 3PL model
mirt::itemfit(model_3PL, empirical.plot = 1)


# Estimate the ability parameters for Rasch and save as a vector
theta_rasch <- as.vector(mirt::fscores(model_rasch))

# Use the difficulty and ability parameters to create an item-person map
ShinyItemAnalysis::ggWrightMap(theta = theta_rasch,
                               b = param_rasch$b,
                               item.names = colnames(sapa_clean), # item names (optional)
                               color = "lightblue", # color of ability distribution (optional)
                               ylab.theta = "Latent Trait", # label for ability distribution (optional)
                               ylab.b = "Item Difficulty") # label for item difficulty (optional)

# Estimate ability parameters
theta_3PL <- mirt::fscores(model_3PL, # estimated IRT model
                           method = "EAP", # estimation method
                           full.scores.SE = TRUE) # return the standard errors

# See the estimated ability parameters
head(theta_3PL)

# See the distribution of the estimated ability parameters
# (i.e., first column) in theta_3PL
hist(theta_3PL[, 1], 
     xlab = "Theta", # label for the x axis
     main = "Ability Distribution") # title for the plot


# Empirical reliability
mirt::empirical_rxx(theta_3PL)


#--------- Do not run this section ----------#
# Partial Credit Model (PCM)
model_pcm <- mirt::mirt(data = sapa_clean,
                        model = 1,
                        itemtype = "Rasch")


# Graded Response Model (GRM)
model_grm <- mirt::mirt(data = sapa_clean,
                        model = 1,
                        itemtype = "graded")

