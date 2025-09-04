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



###--------------------if Github token expired ------follow below steps 2 --------


usethis::use_git_remote(
  name = "origin",
  url = "https://github.com/JohnsonAcharya/R-LearnHubBox.git"
)

# Then check:
system("git remote -v")


# If you've committed changes locally, you can push them:
usethis::git_push()


#Option 1: Use Git via the Terminal

#  Use this in Terminal:  git push origin master 

# Replace main with your branch name if it’s different (master, dev, etc.)

# If you're using RStudio, go to the Terminal tab and run:


# Test change to trigger push
# Test check again changes to trigger pust button 
