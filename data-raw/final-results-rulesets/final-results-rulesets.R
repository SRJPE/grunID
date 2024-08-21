library(dplyr)
library(tidyr)
library(readxl)
library(here)


# 1)  Compile all sample data and genetic data into PLAD Data format
# 2)  Remove individuals with no genotype data (no SH or GT)
# 3)  Remove individuals that have only GT-seq Pop Structure results (no Ots28 call from either SH or GT-seq)
# 4)  Remove individuals with discordant Ots28 calls (SH vs GT)
# 5)  Remove SH Ots28 Heterozygotes with no associated GT-seq Population Structure data
# 6)  Remove SH Ots16 Heterozygotes (S-W HETEROZYGOTES) with no associated GT-seq data
# 7)  Remove SH Ots28 Early individuals with no SH Ots16 result and no GT-seq results (previously designated SPRING OR WINTER)
# 8)  Ots28 Heterozygotes with Population Structure assignment probabilities lower than 0.8 are assigned as “UNKNOWN”
# 9)  Remove individuals with otherwise problematic discordance
# - Examples:
#   SH Spring - GTseq SpW Winter - Pop Structure Fall
# No SH - GTseq SpW Winter - Pop Structure Fall
# SH S-W Heterozygote - No GT Ots28 - Pop Structure Fall (< 0.8)
#
# 10) [2024 JPE only] Remove (temporarily) SH Ots28 and Ots16 Heterozygotes that have not yet been sequenced/genotyped with GT-seq (n = 116)
# - Make final calls when GT-seq processing is complete

all_results <- readxl::read_excel(here("data-raw/final-results-rulesets/2024_JPE_Genetic_Results_07-02-2024.xlsx"))
