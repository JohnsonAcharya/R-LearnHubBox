


library(readr)
library(dplyr)
library(stringr)

# Load the CSV file
df <- read_csv("bank_statement.csv")

glimpse(df)

str(df)

head(df)
glimpse(df)

extract_clean_name <- function(narration) {
  narration <- str_to_upper(narration)
  
  patterns <- list(
    # Match names before 'WORLD WIDE MEDIA'
    "-([A-Z ]+?)-WORLD ?WIDE MEDIA",
    
    # Match names before 'NETBANK'
    "-([A-Z ]+?)-NETBANK",
    
    # Match names between bank code and transaction ref
    "NEFT(?: DR| CR)?-[A-Z0-9]+-([A-Z ]+?)-[A-Z]{4}[N0-9]+",
    
    # CHQ DEP entries
    "CHQ DEP.*?:\\s*([A-Z0-9 ]+?)\\s*:\\s*[A-Z ]+ BANK",
    
    # Match 11+ digit account ref with name
    "\\d{11,}-([A-Z ]+)",
    
    # TPT entries with name after last hyphen
    "TPT-[^-]+-([A-Z ]+)"
  )
  
  for (pat in patterns) {
    m <- str_match(narration, pat)[, 2]
    if (!is.na(m) && str_count(m, " ") >= 1) {
      return(str_squish(m))
    }
  }
  
  return(NA)
}


# Apply the function, but DO NOT filter
df <- df %>%
  mutate(Extracted_Name = sapply(Narration, extract_clean_name))

# Output: all 2067 rows retained
print(nrow(df))  # Should print 2067
# Optional: write to file
write_csv(df, "cleaned_extracted_names4.csv")
