## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(REDCapQC)

## ----run----------------------------------------------------------------------
# Run script
dir = "/Users/goings/Documents/Classes/B581 Biostatistics Computing/Final Project"
setwd(dir)
querylist <- REDCapQC(getwd(), "MIND_20231105.Rda")

# Summary of queries generated
table(querylist$var, querylist$txt)
head(querylist)

