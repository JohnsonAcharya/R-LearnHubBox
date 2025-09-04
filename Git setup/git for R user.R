## Steps to use GitHub in R

# If already installed Git, Setup Done




install.packages("usethis")
library(usethis)

##  Initialize Git
use_git()  # Next follow the steps

# Select the option

library(gitcreds)
gitcreds_set()
 
## abort with error or replace the credentials with existing or replace with updated token from github password

use_github()
#this will create Github repo with your project Name in Github



###--------------------if Github token expired ------follow below steps--------


### connect your local R project to your existing GitHub repository instead of trying to create a new one.

###Step-by-Step: Connect to Existing GitHub Repo


##   <<<<< Manually Set the GitHub Remote >>>>

## Since the GitHub repo already exists, you need to set the remote URL manually.

system('git remote add origin https://github.com/GaneshAc/R-LearnHubBox.git')

## Then verify:
system('git remote -v')

###  Push Local Code to GitHub

##  Now push your code to the GitHub repository:
system('git push -u origin main')

## if your local branch is named master instead of main, use:
system('git push -u origin master')

