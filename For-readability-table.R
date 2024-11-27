library(dplyr) # Data manipulation
library(DT) #Interactive tables
library(pdftools)
library(quanteda.textstats) # analysis tool for readability

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

dmv_ob <- pdftools::pdf_text("10-01-24-Passed-accessible-DL-600-Rev.-1-2024.pdf")
# Remove extra whitespace and empty lines
dmv_ob_cleaned <- sapply(dmv_ob, function(x) {
  lines <- unlist(strsplit(x, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  paste(lines, collapse = "\n")
})

# View the cleaned character vector
print(dmv_ob_cleaned)

dmv_r_df <- textstat_readability(dmv_ob_cleaned, measure = c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade"))

dmv_r_df <- dmv_r_df %>%
  slice(-(1:6)) %>%
  slice(-(85:86))

dmv_averages <- dmv_r_df %>%
  summarise(across(c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade"), mean, .names = "avg_{.col}")) %>%
  mutate(Overall_avg = rowMeans(across(avg_Flesch.Kincaid:avg_Coleman.Liau.grade)))

dmv_round_ave <- round_df(dmv_averages)

# View result
print(dmv_averages)
print(dmv_round_ave)

rev_lab_pdf <- pdf_text("What is PCR_ - HackMD.pdf")
rev_lab_pdf_c <- sapply(rev_lab_pdf, function(x) {
  lines <- unlist(strsplit(x, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  paste(lines, collapse = "\n")
})

rev_lab_df <- textstat_readability(rev_lab_pdf_c, measure = c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade"))

rev_lab_averages <- rev_lab_df %>%
  summarise(across(c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade"), mean, .names = "avg_{.col}")) %>%
  mutate(Overall_avg = rowMeans(across(avg_Flesch.Kincaid:avg_Coleman.Liau.grade))) %>%
  mutate(Document = paste("My revised PCR page"), .before=avg_Flesch)

rev_lab_round_averages <- round_df(rev_lab_averages)

print(rev_lab_averages)
print(rev_lab_round_averages)

lab_pdf <- pdf_text("~/mcb120/Experiment 4 Manual_F24.pdf")

lab_pdf_c <- sapply(lab_pdf, function(x) {
  lines <- unlist(strsplit(x, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  paste(lines, collapse = "\n")
})

lab_df <- textstat_readability(lab_pdf_c, measure = c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade"))

lab_averages <- lab_df %>%
  summarise(across(c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade"), mean, .names = "avg_{.col}")) %>%
  mutate(Overall_avg = rowMeans(across(avg_Flesch.Kincaid:avg_Coleman.Liau.grade))) %>%
  mutate(Document = paste("Lab's handout"), .before=avg_Flesch)

lab_round_averages <- round_df(lab_averages)

pcr_page_df <- lab_df %>%
  slice(3)

pcr_page_averages <- pcr_page_df %>%
  summarise(across(c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade"), mean, .names = "avg_{.col}")) %>%
  mutate(Overall_avg = rowMeans(across(avg_Flesch.Kincaid:avg_Coleman.Liau.grade))) %>%
  mutate(Document = paste("Lab's PCR page"), .before=avg_Flesch)

pcr_page_round_averages <- round_df(pcr_page_averages)

#put all data frames into list
df_list <- list(lab_round_averages, pcr_page_round_averages, rev_lab_round_averages)

#merge all data frames together
Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 
