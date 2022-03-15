
# Install all the packages together
install.packages(c("dplyr", "car", "skimr", "DataExplorer", "ggcorrplot",
                   "psych", "CTT", "ShinyItemAnalysis", "devtools", "rmarkdown"))

# Or, we could do it one by one. For example:
# # install.packages("CTT")
# # install.packages("psych")
# # and so on...

devtools::install_github("zief0002/QME")

# Activate the required packages
library("dplyr")
library("car")
library("skimr")
library("DataExplorer")
library("ggcorrplot")
library("psych")
library("CTT")
library("ShinyItemAnalysis")
library("QME")

# Set the working directory to wherever you are going to keep the files
setwd("C:/Users/Okan/Desktop/CTT Analysis")


#----------------------------------------------------------------------------
#                                                                           #
#                           Example 1: NFC Scale                            #
#                                                                           #
#---------------------------------------------------------------------------#

nfc <- read.csv("nfc_data.csv", header = TRUE)
head(nfc)
str(nfc)

# Check out the data
DataExplorer::introduce(nfc)

DataExplorer::plot_intro(nfc)

DataExplorer::plot_missing(nfc)

# Categorical variables (bar plots)
DataExplorer::plot_bar(data = nfc[, c("education", "sex")])

# Continuous variables (histogram)
DataExplorer::plot_histogram(data = nfc[, c("age", "self_control", "action_orientation", "effortful_control")])

# Continuous variables (boxplot) by a categorical variable
DataExplorer::plot_boxplot(data = nfc[!is.na(nfc$sex), # select cases where sex is not missing
                                      # Select variables of interest
                                      c("sex", "self_control", "action_orientation", "effortful_control")], 
                           by = "sex") # Draw boxplots by sex (a categorical variable)


# Drop the id variable so it doesn't get analyzed with the other variables
nfc2 <- DataExplorer::drop_columns(nfc, "id")

# This code creates a report and saves it into the working directory
DataExplorer::create_report(data = nfc2,
                            report_title = "NFC Scale Analysis",
                            output_file = "nfc_report.html")


skimr::skim(nfc)

psych::describe(x = nfc)

response <- dplyr::select(nfc, # name of the dataset
                          starts_with("nfc")) # variables to be selected

head(response)

cormat <- psych::polychoric(x = response)$rho
print(cormat)

ggcorrplot::ggcorrplot(corr = cormat, # correlation matrix
                       type = "lower", # print only the lower part of the correlation matrix
                       show.diag = TRUE, # show the diagonal values of 1
                       lab = TRUE, # add correlation values as labels
                       lab_size = 3) # Size of the labels


# -1 means reverse-code the item
nfc_key <- c(1,1,1,-1,1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,-1)

response_recoded <- psych::reverse.code(
  keys = nfc_key, # reverse-coding key
  items = response, # dataset to be transformed
  mini = 1, # minimum response value
  maxi = 7) # maximum response value


cormat_recoded <- psych::polychoric(response_recoded)$rho

ggcorrplot::ggcorrplot(corr = cormat_recoded,
                       type = "lower", 
                       show.diag = TRUE,
                       lab = TRUE, 
                       lab_size = 3) 


# Rename the columns
colnames(response_recoded) <- colnames(response)

# Save the data as a data frame
response_recoded <- as.data.frame(response_recoded)

# Preview the first six rows of the data
head(response_recoded)

# Run the item analysis and save it as itemanalysis_ctt
itemanalysis_ctt <- CTT::itemAnalysis(items = response_recoded, pBisFlag = .2, bisFlag = .2)

# Print the item report
itemanalysis_ctt$itemReport

# Run the item analysis and save it as itemanalysis_psych
itemanalysis_psych <- psych::alpha(x = response_recoded)

# Print the results
itemanalysis_psych

# Run the item analysis and save it as itemanalysis_shiny
itemanalysis_shiny <- ShinyItemAnalysis::ItemAnalysis(Data = response_recoded)

# Print the results
itemanalysis_shiny

# Create a difficulty and discrimination (DD) plot
ShinyItemAnalysis::DDplot(Data = response_recoded, discrim = "RIR")

# Split-half reliability
split_half <- function(data, type = "alternate", seed = 2022) {
  
  # Select every other item
  if (type == "alternate") {
    first_half <- data[, seq(1, ncol(data), by = 2)]
    second_half <- data[, seq(2, ncol(data), by = 2)]
    first_total <- rowSums(first_half, na.rm = T)
    second_total <- rowSums(second_half, na.rm = T)
    rel <- round(cor(first_total, second_total), 3)} else
  
  # Select two halves randomly
  if (type == "random") {
    set.seed(seed)
    num_items <- 1:ncol(data)
    first_items <- sample(num_items, round(ncol(data)/2, 0))
    first_half <- data[, first_items]
    second_half <- data[, -first_items]
    first_total <- rowSums(first_half, na.rm = T)
    second_total <- rowSums(second_half, na.rm = T)
    rel <- round(cor(first_total, second_total), 3)}
  
  return(rel)
}


split_half(data = response_recoded, type = "alternate")
split_half(data = response_recoded, type = "random", seed = 2022)

psych::splitHalf(r = response_recoded)


# Save the reliability results as sp_rel
sp_rel <- psych::splitHalf(r = response_recoded, raw = TRUE)

hist(x = sp_rel$raw, # extract the raw reliability values saved in sp_rel
     breaks = 101, # the number of breakpoints between histogram cells
     xlab = "Split-Half Reliability", # label for the x-axis
     main = "All Split-Half Reliabilities for the NFC Scale") # title for the histogram

# Add a red, vertical line showing the mean
# here v is a (v)ertical line
# col is the colour of the line
# lwd is the width of the line (the larger, the thicker line)
# lty is the line type (1 = solid, 2 = dashed, etc.)
# See http://www.sthda.com/english/wiki/line-types-in-r-lty for more details
abline(v = mean(sp_rel$raw), col = "red", lwd = 2, lty = 2)


# Print the results
itemanalysis_ctt

# Try the QME package
reliability_qme <- QME::analyze(test = response_recoded, id = FALSE, na_to_0 = FALSE)
reliability_qme

# Create a copy of the original response dataset
response_experiment <- response_recoded

# Set the seed to control random number generation
set.seed(seed = 2525)

# Generate random integer values between 1 and 7 and replace the original responses with them
response_experiment$nfc01 <- sample.int(n = 7, size = nrow(response_experiment), replace = TRUE)
response_experiment$nfc02 <- sample.int(n = 7, size = nrow(response_experiment), replace = TRUE)

# Check the correlations among the items using the new dataset
cormat_experiment <- psych::polychoric(response_experiment)$rho

ggcorrplot::ggcorrplot(corr = cormat_experiment,
                       type = "lower", 
                       show.diag = TRUE,
                       lab = TRUE, 
                       lab_size = 3) 

# Now check the internal consistency
QME::analyze(test = response_experiment, id = FALSE, na_to_0 = FALSE)

# Spearman-Brown formula
CTT::spearman.brown(r.xx = 0.87, input = 0.5, n.or.r = "n")

# Save the result as n
n <- CTT::spearman.brown(r.xx = 0.87, input = 0.90, n.or.r = "r")

# Multiply it with the current length and round it with zero digits
round(n$n.new * 16, digits = 0)

# Our custom recode function
recode_nfc <- function(x) {
  car::recode(x, "1=-3; 2=-2; 3=-1; 4=0; 5=1; 6=2; 7=3")
}

# Recode the responses
response_recoded <- apply(response_recoded, # data to be modified
                          2, # 2 to apply a function to each column (or 1 for each row)
                          recode_nfc) # Function to be applied


# Compute the NFC Scale scores
nfc_score <- rowSums(response_recoded)

# Summarize the scores
summary(nfc_score)

# Visualize the distribution of the NFC Scale scores
hist(nfc_score,
     xlab = "NFC Scale Score",
     main = "Distribution of the NFC Scale Scores")


scores <- cbind(nfc_score, nfc[, c("action_orientation", "self_control", "effortful_control")])


cormat_scores <- cor(scores, use = "pairwise.complete.obs")

print(cormat_scores)

ggcorrplot::ggcorrplot(corr = cormat_scores,
                       type = "lower",
                       show.diag = TRUE,
                       lab = TRUE,
                       lab_size = 3)


psych::correct.cor(x = cormat_scores, # Raw correlation matrix
                   y = c(0.87, 0.791, 0.817, 0.783)) # Reliability values in the same order as cormat_scores


# Custom function for calculating item-validity index
ivi <- function(item, criterion) {
    s_i <- sd(item, na.rm = TRUE)
    r <- cor(item, criterion, use = "complete.obs")
    index <- s_i * r
    return(index)
}

# Apply the ivi function to the NFC items
nfc_ivi <- apply(response_recoded, 
                 2,                
                 function(x) ivi(item = x, criterion = nfc$action_orientation))

# Save the results as a data frame and print them
nfc_ivi <- as.data.frame(nfc_ivi)
print(nfc_ivi)


#----------------------------------------------------------------------------
#                                                                           #
#                     Example 2: Multiple-Choice Test                       #
#                                                                           #
#---------------------------------------------------------------------------#


# Import the dataset
hci <- read.csv("hci.csv", header = TRUE)

# Print the first 6 rows of the dataset
# You can use head(hci, 10) to print the first 10 rows instead of 6 (default)
head(hci)

# See the structure of the dataset
str(hci)


DataExplorer::plot_bar(data = hci, nrow = 6, ncol = 4)

DataExplorer::plot_histogram(data = hci[, c("study_year")])

# Import the answer key
key <- read.csv("hci_key.csv", header = TRUE)

# Print the answer key
print(key)

# Select the items
hci_items <- dplyr::select(hci, # name of the dataset
                           starts_with("item")) # variables to be selected

head(hci_items)

CTT::distractorAnalysis(items = hci_items, key = key)

# Import the new answer key
key2 <- read.csv("hci_key2.csv" , header = TRUE)

# View the new answer key
print(key2)

# Conduct analysis with analyze() and save the results
hci_analysis <- QME::analyze(test = hci_items, key = key2, id = FALSE)

# Create a distractor report
QME::distractor_report(x = hci_analysis)

# Save the key column from the key data frame as a vector
key3 <- as.vector(key$key)

# Create a distractor functioning plot
ShinyItemAnalysis::plotDistractorAnalysis(Data = hci_items, # response data to be analyzed
                                          key = key3, # answer key
                                          num.groups = 3, # how many student groups we want (default is 3)
                                          item = 1) # the number of the item to be plotted

# Score the items
hci_scored <- CTT::score(items = hci_items, # data to be scored
                         key = key3, # answer key
                         output.scored = TRUE, # save the scored items
                         rel = TRUE) # calculate reliability for the scored items

# Now let's see what's in hci_scored
str(hci_scored)

# Save the scores as a separate variable
scores <- hci_scored$score

# Summarize the scores
summary(scores)

# Visualize the distribution of the scores using a histogram
# We will also customize the histogram a little bit
hist(x = scores, # scores to be visualized
     xlab = "Total Score", # label for the x axis
     main = "Distribution of HCI Total Scores", # title for the plot
     col = "lightblue", # colour for the bars in the histogram
     breaks = 15, # number of breakpoints for the histogram
     xlim = c(0, 20)) # minimum and maximum values for the x axis

# Add a red, vertical line showing the mean
abline(v = mean(scores), col = "red", lwd = 2, lty = 2)

# Print reliability
print(hci_scored$reliability)

# Save the scored items
hci_items_scored <- hci_scored$scored

# Print the first six rows of this dataset
head(hci_items_scored)

# Rescale the scores
scores_scaled <- CTT::score.transform(scores = scores, # scores to be rescaled
                                      mu.new = 50, # mean of the new scale
                                      sd.new = 15) # standard deviation of the new scale


# Visualize the distribution of the scaled scores using a histogram
hist(x = scores_scaled$new.scores,
     xlab = "Scaled Score", 
     main = "Distribution of HCI Scaled Scores", 
     col = "lightblue", 
     xlim = c(0, 100))

# Add a red, vertical line showing the mean
abline(v = mean(scores_scaled$new.scores), col = "red", lwd = 2, lty = 2)

scores_scaled2 <- CTT::score.transform(scores = scores, # scores to be rescaled
                                       mu.new = 50, # mean of the new scale
                                       sd.new = 15, # standard deviation of the new scale
                                       normalize = TRUE) # If TRUE, normalize the scores

# Visualize the distribution of the normalized scaled scores using a histogram
hist(x = scores_scaled2$new.scores,
     xlab = "Normalized Scaled Score", 
     main = "Distribution of HCI Scaled Scores", 
     col = "lightblue", 
     xlim = c(0, 100))

# Add a red, vertical line showing the mean
abline(v = mean(scores_scaled2$new.scores), col = "red", lwd = 2, lty = 2)


cormat_hci <- psych::tetrachoric(x = hci_items_scored)$rho

ggcorrplot::ggcorrplot(corr = cormat_hci,
                       type = "lower", 
                       show.diag = TRUE,
                       lab = TRUE, 
                       lab_size = 3) 


# Run the item analysis and save it as hci_itemanalysis
hci_itemanalysis <- CTT::itemAnalysis(items = hci_items_scored, pBisFlag = .2, bisFlag = .2)

# Print the reliability
hci_itemanalysis

# Print the item report
hci_itemanalysis$itemReport


# Run the item analysis and save it as hci_itemanalysis2
hci_itemanalysis2 <- CTT::itemAnalysis(items = hci_items_scored[, -c(7, 17)], 
                                       pBisFlag = .2, 
                                       bisFlag = .2)

# Print the reliability
hci_itemanalysis2

# Print the item report
hci_itemanalysis2$itemReport


# Function for calculating SEM
# x is the response data
# ci.level is the confidence level we want
sem <- function(x, ci.level = 0.95) {
  require("CTT")
  rxx <- CTT::itemAnalysis(items = x)$alpha
  scores <- rowSums(x, na.rm = TRUE)
  sigma <- sd(scores, na.rm = TRUE)
  sem <- sigma*sqrt((1-rxx))
  z <- qnorm(1-(1-ci.level)/2)
  
  cat("****************************************************","\n")
  cat("","\n")
  cat(sprintf("%40s","Standard Error of Measurement"),"\n")
  cat("","\n")
  cat("****************************************************","\n")
  cat("","\n")
  cat("Coefficient alpha = ", rxx, "\n")
  cat("","\n")
  cat("Standard deviation = ", sigma, "\n")
  cat("","\n")
  cat("SEM = ", sem, "\n")
  cat("","\n")
  cat("To calculate confidence intervals with SEM, use:", "\n")
  cat("","\n")
  cat("Observed Score ± (", sem, "*", z, ")", "\n")
  cat("","\n")
  
  output <- data.frame(lower_CI = scores - (sem*z),
                       observed = scores,
                       upper_CI = scores + (sem*z))
  
  return(output)
}

# Calculate SEM
sem_hci <- sem(x = hci_items_scored, ci.level = 0.95)

# See observed scores and their lower/upper confidence intervals
head(sem_hci)
