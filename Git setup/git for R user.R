## Steps to use GitHub in R

# If already installed Git, Setup Done


install.packages("usethis")
library(usethis)


use_git()  # Next follow the steps

# Select the option

library(gitcreds)
gitcreds_set()
 
## Sbort with error or replace the credentials with existing or replace with updated token from github password

use_github()
#this will create Github repo with your project Name in Github