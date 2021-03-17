######### RUN THE CODE BELOW IN R. R-STUDIO IS THE RECOMMENDED IDE. BOTH R AND R-STUDIO ARE FREE.
######### QUESTIONS SHOULD BE POSTED TO PIAZZA
######### THE ACTUAL ASSIGNMENT BEGINS ON LINE 71


### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data
# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data structure
head(foo)

# one thing to be very careful with (in this data set) is the use of dates. 8 columns involve dates.

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)
names(foo)
# these columns need some tweaking--I want to address missing values, calling the blank (empty) 
# elements "NA" instead of leaving them blank, and I wish to tell R these are "Date" objects.

for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.
  
{
  
  # identify which values are missing in the "i"th column of the foo data set
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  
  # those values that are missing (blank) in the "i"th column are replaced by <NA>
  # because R knows how to handle "NA" -- NA means something special in R--blanks are handled 
  # more unpredictably (which is bad).
  foo[which_values_are_missing, i] <- NA
  
  # last step--replace each of these columns (which is structured as a column of "factor" values)
  # as a column of dates--i.e., convert them to an object of "class" = Date. They are dates, after all.
  # And if you convert them to the Date class, R will know they are dates and you can manipulate 
  # dates in a simple, straightforward way. Otherwise, you won't be able to easily manipulate them
  # arithmetically.  E.g., for simple Date operations, see lines 48-58 below...
  # **By the way, if you don't understand what a "factor" is in R, you should Google it.** 
  foo[, i] <- as.Date(as.character(foo[, i]))
  
}

# Now R knows that these columns are comprised of dates
# for example...  Replicate this yourself...

# foo[3,12]
# [1] "1968-03-13"

# foo[4,12]
# [1] "1968-07-03"

# foo[3,12] - foo[4,12]
# Time difference of -112 days

# Also, one additional helpful hint... How to eliminate rows with NAs...
# The "is.na" function--for more info, Google it or type ?is.na at the R command prompt in the console.
# which.have.NAs <- which(is.na(foo$Rating == TRUE)) # for which rows is the claim "is.na" a TRUE claim?

# Then, if you wanted to, e.g., remove all those rows, retaining only the rows with ratings...
# new_foo <- foo[-which.have.NAs, ]
# Notice I called this tweaked data set "new_foo" instead of rewriting over the original data set...
# It's a bit safer to do this, in case I decide I want to quickly revert back to the original data set.

###########################################################################

### ASSIGNMENT 1 -- You may want to read ALL the questions before you begin. 
### NOTE: FOR ALL QUESTIONS BELOW, ONLY CONSIDER PROJECTS WITH 
### non-missing "Circulation.Date" >= 2008-01-01. 
### EXCLUDE ALL OTHER PROJECTS FROM YOUR ANALYSIS.
### YOU MUST provide a link to your R code. ------ DON'T FORGET TO DO THIS!!!!!!!!!!!!
# Take note of the column names: i.e., you can type: names(foo)
# fyi: the column called "Rating" is the success rating at completion. 0 = lowest, 3 = highest.

# (1) When projects are approved, they are approved for a certain period of time (until the time of
# "original completion date"). While projects are active, this "original" completion date is 
# often pushed out (extended), and then there is a "revised" completion date.

# You have been told that project duration at approval is generally about 
# 2 years (24 months). In other words, (purportedly) when projects are approved, the difference 
# between the original project completion date and the the approval date is (supposedly) 
# approximately 24 months. 

# (a) Is this claim true? Explain. (Remember, for this ENTIRE assignment, only consider 
# projects with Circulation.Date >= 2008-01-01. This will be your only reminder...)
# exclude missing values for circulation date
foo <- foo[!is.na(foo$CirculationDate),]
# extract projects with Circulation.Date >= 2008-01-01
which_projects <- which(foo$CirculationDate < as.Date('2008-01-01'))
foo <- foo[-which_projects,]
dim(foo)
# drop projects with missing values for approval dates OR original project completion date
new_foo <- foo[!is.na(foo$OriginalCompletionDate),]
new_foo <- new_foo[!is.na(new_foo$ApprovalDate),]
dim(new_foo)
# check the claim by constructing confidence interval for the mean of duration at approval
duration.at.approval <- as.numeric(new_foo$OriginalCompletionDate-new_foo$ApprovalDate, unit='days')/30
#### test with Nhat
#duration.at.approval <- as.numeric(new_foo$OriginalCompletionDate-new_foo$ApprovalDate, unit='days')

#### end test
## histogram to inspect skewness
hist(duration.at.approval, main='Histogram for durations at approval',
     xlab="Duration (months)",
     ylab="Frequency (counts)",
     col="gray",
     labels=T,
     right=F)
## Confidence interval
s <- sd(duration.at.approval) # standard deviation
n <- nrow(new_foo) # number of observations
point_estimate <- mean(duration.at.approval)
SE <- s/sqrt(n)
t <- qt(0.975, df=n-1)
lower <- point_estimate - t*SE
upper <- point_estimate + t*SE
lower
upper
point_estimate 

# Has project duration at approval changed over time (consider projects circulated earlier
# and circulated later). Be sure to discuss mean durations, median durations, and the
# interquartile range of durations (using the "quantile" function). 
# Approximate suggested length: 3-5 sentences
# get 20% earliest projects and 20% latest projects
sorted_new_foo <- new_foo[order(new_foo$CirculationDate),]
early.projects <- sorted_new_foo[1:(nrow(new_foo)/5),]
late.projects <- sorted_new_foo[(nrow(new_foo)*.8):nrow(new_foo),]
### test with Nhat
#early.duration <- as.numeric(early.projects$OriginalCompletionDate-early.projects$ApprovalDate)
#late.duration <- as.numeric(late.projects$OriginalCompletionDate-late.projects$ApprovalDate)
#### end test
early.duration <- as.numeric(early.projects$OriginalCompletionDate-early.projects$ApprovalDate)/30
late.duration <- as.numeric(late.projects$OriginalCompletionDate-late.projects$ApprovalDate)/30
mean(early.duration)
mean(late.duration)
median(early.duration)
median(late.duration)
IQR(early.duration)
IQR(late.duration)
sd(early.duration)
sd(late.duration)
plot(density(early.duration), col = "red", lwd = 3,
     xlab='Duration at approval (months)')
lines(density(late.duration), col = "blue", lwd = 3)



# (b) How does original planned project duration differ from actual duration (if actual duration is 
# measured as the duration between "ApprovalDate" and "RevisedCompletionDate"?)  Once again, use
# means, medians, and interquartile ranges to explain your results. 
# Approximate suggested length: 3-5 sentences
new_foo <- foo[!is.na(foo$RevisedCompletionDate),]
new_foo <- new_foo[!is.na(new_foo$ApprovalDate),]
dim(new_foo)

duration.revised <- as.numeric(new_foo$RevisedCompletionDate-new_foo$ApprovalDate, unit='days')/30
### test with Nhat
# duration.revised <- as.numeric(new_foo$RevisedCompletionDate-new_foo$ApprovalDate, unit='days')

#### end test
mean(duration.at.approval)
mean(duration.revised)
median(duration.at.approval)
median(duration.revised)
IQR(duration.at.approval)
IQR(duration.revised)
IQR(duration.revised) - IQR(duration.at.approval)
plot(density(duration.at.approval), col = "red", lwd = 3,
     xlab='Duration (months)')
lines(density(duration.revised), col = "blue", lwd = 3)
# (2) What % of projects that have ratings were rated 0?
# What % were rated 1? What % were rated 2? What % were rated 3? Answer these questions using a table
# or a figure. Provide a title AND an explanatory sentence or two that provides the numerical % results
# rounded to the nearest percentage-point.

# remove projects that have missing value for ratings
new_foo <- foo[!is.na(foo$Rating),]
dim(new_foo)
a <- 0
percentages <- c(round(sum(new_foo$Rating==0)/nrow(new_foo)*100,digits=0),
                 round(sum(new_foo$Rating==1)/nrow(new_foo)*100,digits=0),
                 round(sum(new_foo$Rating==2)/nrow(new_foo)*100,digits=0),
                 round(sum(new_foo$Rating==3)/nrow(new_foo)*100,digits=0))

barplot(percentages, ylim=c(0,100), 
        space=c(1,1),
        ylab = 'Proportion (%)',
        xlab = 'Rating',
        main = 'Proportions of each rating',
        names.arg = c("0"," 1","2", "3")
)
text(1.5, 6, paste(percentages[1],"%"))
text(3.5, 19, paste(percentages[2],"%"))
text(5.5, 71, paste(percentages[3],"%"))
text(7.5, 16, paste(percentages[4],"%"))
# (3) Repeat problem 2, but this time exclude all PPTA projects. PPTA projects are more prone to 
# negative ratings, because after a certain point in time only the low-rated PPTA projects required
# ratings.  PPTA stands for "Project Preparatory Technical Assistance" and it is basically a project
# intended to set up a loan (often a very large multi-million-dollar loan). Only PPTAs that fail to 
# "eventuate" to a loan are rated, which is why they are usually rated negatively.

# remove projects that have missing value for ratings
new_foo <- foo[!is.na(foo$Rating),]
dim(new_foo)
# remove PPTA projects
new_foo <- new_foo[-which(new_foo$Type=='PPTA'),]

percentages <- c(round(sum(new_foo$Rating==0)/nrow(new_foo)*100,digits=0),
                 round(sum(new_foo$Rating==1)/nrow(new_foo)*100,digits=0),
                 round(sum(new_foo$Rating==2)/nrow(new_foo)*100,digits=0),
                 round(sum(new_foo$Rating==3)/nrow(new_foo)*100,digits=0))

barplot(percentages, ylim=c(0,100), 
        space=c(1,1),
        ylab = 'Proportion (%)',
        xlab = 'Rating',
        main = 'Proportion of each rating',
        names.arg = c("0"," 1","2", "3")
)
text(1.5, 5, paste(percentages[1],"%"))
text(3.5, 17, paste(percentages[2],"%"))
text(5.5, 73, paste(percentages[3],"%"))
text(7.5, 17, paste(percentages[4],"%"))

# (4) Identify the top 25% of projects by "Revised.Amount" and the bottom 25% of projects by 
# "RevisedAmount". ("RevisedAmount" shows the final project budget.)
# Compare the ratings of these projects. Can you draw a causal conclusion about the effect of 
# budget size on ratings? Why or why not? 
# Hint: Compare the characteristics of the two project groupings,
# e.g., "Dept", "Division", "Cluster", "Country"
# Approximate suggested length: 3-5 sentences.

# remove projects with missing values for Revised.Amound
new_foo <- foo[!is.na(foo$RevisedAmount),]
# further removes projects with missing values for Ratin
new_foo <- new_foo[!is.na(foo$Rating),]

# top 25%
n <- 25
top.projects <- new_foo[new_foo$RevisedAmount > quantile(new_foo$RevisedAmount, prob=1-n/100),]
bottom.projects <- new_foo[new_foo$RevisedAmount < quantile(new_foo$RevisedAmount, prob=n/100),]

dim(top.projects)
dim(bottom.projects)
mean(top.projects$Rating)
mean(bottom.projects$Rating)

sort(table(bottom.projects$Division))  
sort(table(top.projects$Division))

sort(table(bottom.projects$Prefix))  
sort(table(top.projects$Prefix))



names(top.projects)
# (5) Imagine your manager asks you to apply Jeremy Howard's drivetrain model to the 
# problem of optimal budget-setting to maximize project success (i.e., "Rating"). 
# In such a situation, what would be the:
# (a) decision problem or objective?
# (b) lever or levers?
# (c) ideal RCT design?
# (d) dependent variable(s) and independent variable(s) in the modeler
# (e) And---Why would running RCTs and modeling/optimizing over RCT results be preferable 
# to using (observational, non-RCT) "foo" data?
# Approximate suggested length: 1-3 sentences for each sub-question.
