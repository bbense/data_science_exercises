#
#
# Run commands to complete data cleaning exercise 1

is_thing <- function(cmp, thing){
  if(cmp == thing) {
  1
  } else {
  0
  }
}

find_category <- function(code) {
  switch(code,
    "p" = "smartphone",
    "x" = "Laptop",
    "v" = "TV",
    "q" = "Tablet"
  )
}


library(readr)
refine_original <- read_csv("~/springboard/data_science_exercises/cleaning/refine_original.csv")
View(refine_original)
install.packages("tidyr")
library(dplyr)
library(tidyr)

brands <- refine_original %>%
  mutate(company = tolower(company)) %>%
  mutate(company = gsub("ak.*$", "akzo", company)) %>%
  mutate(company = gsub(".*ps$", "philips", company))  %>%
  mutate(company = gsub("un.*$", "unilever", company))

companies <- brands %>%
  separate(`Product code / number`, c("Product Code", "Product Number"), sep = "-")  %>%
  mutate(Category = sapply(`Product Code`,find_category)) %>%
  mutate(full_address = paste(address, city, country, sep = ",", collapse = NULL))  %>%
  mutate(company_van_houten = sapply(company, is_thing, thing="van houten"))  %>%
  mutate(company_philips = sapply(company, is_thing, thing="philips"))  %>%
  mutate(company_akzo = sapply(company, is_thing, thing="akzo"))  %>%
  mutate(company_unilever = sapply(company, is_thing, thing="unilever"))

final_version <- companies %>%
  mutate(product_tv = sapply(`Product Code`,is_thing, thing="v")) %>%
  mutate(product_laptop = sapply(`Product Code`,is_thing, thing="x")) %>%
  mutate(product_tablet = sapply(`Product Code`,is_thing, thing="q")) %>%
  mutate(product_smartphone = sapply(`Product Code`,is_thing, thing="p"))

write.csv(final_version, file="final_version.csv")
