
library("DataExplorer")
library("ggcorrplot")
library("psych")
library("lavaan")
library("mirt")

sapa <- read.csv("C:/Users/Okan/Documents/GitHub/edpy507/data_and_codes/sapa_data.csv", header = TRUE)

head(sapa)

str(sapa)

dim(sapa)

DataExplorer::plot_intro(sapa)

DataExplorer::plot_missing(sapa)


cormat <- psych::tetrachoric(x = sapa)$rho

# Create a correlation matrix plot
ggcorrplot::ggcorrplot(corr = cormat, # correlation matrix
                       type = "lower", # print only the lower part of the correlation matrix
                       hc.order = TRUE, # hierarchical clustering
                       show.diag = TRUE, # show the diagonal values of 1
                       lab = TRUE, # add correlation values as labels
                       lab_size = 3) # Size of the labels


# In the "fa" function in the psych package, we need to specify
# r= our data set (either raw data or a correlation matrix)
# n.obs = Number of observations in the data (necessary only when using a correlation matrix)
# nfactors = number of factors that we expect to see in the data
# rotate = type of rotation if n > 1; 
# varimax is an orthogonal rotation that assumes no correlation between factors
# oblimin is an oblique rotation that assumes factors are somewhat correlated
# It is often better to start with "oblimin"
# If the correlations among factors are small, then "varimax" could be used
# fm = the factor analysis method. "pa" is principal axis (typical EFA)
# Other fm types include "wls" for weighted least squares; "minres" for ordinary least squares
# cor = How to find the correlations when using raw data
# If your variables are continuous, use cor="Pearson" (Pearson correlation)
# If your variables are dichotomous, use cor="tet" (tetrachoric correlation)
# If your variables are polytomous, use cor="poly" (polychoric correlation)
# If your variables are mixed (both dichotomous and polytomous), use cor="mixed"
# In mydata, all variables are dichotomous. So, I use cor="tet"


# Try one-factor EFA model --> nfactors=1
efa.model1 <- fa(r=sapa, nfactors=1, rotate="oblimin", fm="pa", cor="tet")
print(efa.model1, sort=TRUE) # Show the factor loadings sorted by absolute value

# Try two-factor EFA model --> nfactors=2
efa.model2 <- fa(sapa, nfactors=2, rotate="oblimin", fm="pa", cor="tet")
print(efa.model2, sort=TRUE) # Show the factor loadings sorted by absolute value
fa.diagram(efa.model2)

# Try one-factor EFA model after excluding the rotation items
response <- subset(sapa, select = -c(rotate.3, rotate.4, rotate.6, rotate.8))

efa.model3 <- fa(response, nfactors=1, rotate="oblimin", fm="pa", cor="tet")
print(efa.model3, sort=TRUE) # Show the factor loadings sorted by absolute value


# If we know the underlying structure in the data, then we can run confirmatory factor analysis (CFA)
# CFA requires the researcher to define the structure (rather than the program determines it)
# Realize that the model definition below begins with a single quote and ends with the same single quote.
mymodel <- '# Define a single factor
            f1 =~ reason.4 + reason.16 + reason.17 + reason.19 + letter.7 + 
            letter.33 + letter.34 + letter.58 + matrix.45 + matrix.46 + 
            matrix.47 + matrix.55'

# Run the CFA model for the model defined above
# If the items are dichotomous or polytomous, then
# estimator should be either "MLR" or "WLSMV" because
# these estimators are more robust against non-normality
# which is usually the case for categorical data

# Now use estimator = "MLR"
cfa_sapa <- lavaan::cfa(mymodel, data=response, estimator = "MLR")
summary(cfa_sapa, fit.measures=TRUE, rsquare = TRUE)


psych::describe(x = sapa)

# Save the correlation matrix
cormat <- psych::tetrachoric(x = response)$rho

# Create a correlation matrix plot
ggcorrplot::ggcorrplot(corr = cormat, # correlation matrix
                       type = "lower", # print only the lower part of the correlation matrix
                       hc.order = TRUE, # hierarchical clustering
                       show.diag = TRUE, # show the diagonal values of 1
                       lab = TRUE, # add correlation values as labels
                       lab_size = 3) # Size of the labels



# Run the item analysis and save it as itemanalysis_psych
itemanalysis_psych <- psych::alpha(x = response)

# Print the results
itemanalysis_psych


# Check local item independence
















