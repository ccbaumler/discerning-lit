library(dplyr) # Data manipulation
library(gt) #Presentation-ready tables
library(bibliometrix)  #Load the database site search results
library(quanteda.textstats) # analysis tool for readability

wos_df <- bibliometrix::convert2df("example-wos.txt")

#scopus_df <- bibliometrix::convert2df("scopus-inkscape.bib", dbsource = "scopus", format = "bibtex")

#bound_df <- rbind(wos_df, scopus_df)


class(wos_df)
glimpse(wos_df)


#create an abstract only character vector
abstracts <- wos_df$AB
class(abstracts)
glimpse(abstracts)

abs_df <- abstracts %>% 
  textstat_readability(measure = c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade")) %>% 
  cbind(wos_df$TI) %>% #add titles
  rename(Titles=8)%>% #add col name
  cbind(wos_df$DI) %>% #add DOIs
  rename(DI=9) %>% #add col name
  na.omit() #remove NA values
class(abs_df)
glimpse(abs_df)

looong <- tidyr::pivot_longer(abs_df, 
                              cols = c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade")
)
class(looong)
glimpse(looong)

#This is a long list of dplyr functions that will highlight 
#the value of learning this type of command piping. 
#This table will display the top ten articles in
#each readability category. These articles will be displayed with the 'most' 
#readable document first in each category.
looong %>% #Use the longer dataframe
  mutate(Titles = sprintf('<a href = "https://www.doi.org/%s">%s</a>', 
                          DI, 
                          Titles),
         Titles = lapply(Titles, gt::html)) %>% #Hyperlink each Title with DOI link
  arrange(factor(name, levels = c("Flesch",
                                  "Flesch.Kincaid",
                                  "FOG",
                                  "SMOG",
                                  "ARI",
                                  "Coleman.Liau.grade")), value) %>%
  group_by(name) %>% #while we could use 
  #`group_by(name) %>% arrange(name, value)`
  #using `arrange(factor())` allows us to
  #set the order of the groups!
  filter(name %in% c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade")) %>%
  filter(row_number() %in% c(1:10, (n() - 9):n())) %>%
            #(name %in% "Flesch" & row_number() %in% (n()-9):n()))) %>% 
  #above we "filtered" the first 10 and bottom 10 rows of corresponding stats
  #remember that we have "arranged" each group by ascending values 
  arrange(value = ifelse(name %in% "Flesch", desc(value),
                         value)) %>% #if the name column contains Flesch
  #"arrange" by decreasing values 
  gt() %>%
  cols_hide(columns = c(document, DI)) %>%
  tab_stubhead(label = "Titles") %>%
  fmt_number(columns = value, decimals = 2)  %>%
  tab_header(
    title = md("The *most* readable documents"),
    subtitle = "Six readability statistics; Flesch, Flesch.Kincaid, FOG, SMOG, ARI, Coleman.Liau.grade"
  ) %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", 
                size = 24)
    )
  )%>%
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    row_group.background.color = "grey") %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", 
                   weight = px(3)),
      cell_text(weight = "bold")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(
        align = "center",
        weight = "bold"
      )
    ),
    locations = list(
      cells_row_groups(
        groups = c("Flesch",
                   "Flesch.Kincaid",
                   "FOG",
                   "SMOG",
                   "ARI",
                   "Coleman.Liau.grade")
      )
    )
  ) %>%
  cols_align(
    align = "left",
    columns = Titles
  ) %>%
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = Titles
    )
  ) %>%
  tab_source_note(
    source_note = md("Source: SCOPUS, Colorectal Metagenome and Metabolome Search")
  ) %>%
  tab_source_note(
    source_note = md(
      'Query: TITLE-ABS-KEY ( "colorectal cancer*"  OR  "colorectal neoplas*"  OR  "adenomatous polyposis coli"  OR  "colon* neoplas*"  OR  "rectal neoplas*"  OR  "hereditary nonpolypo*"  AND  "metagenom*"  AND  "metabol*" )'
    )
  ) %>%
  tab_footnote(
    footnote = md("The **lowest** estimated grade level."),
    locations = cells_body(
      columns = value, 
      rows = value == min(value)
    )
  ) %>%
  opt_footnote_marks(marks = c("*", "+"))

looong %>% #Use the longer dataframe
  mutate(Titles = sprintf('<a href = "https://www.doi.org/%s">%s</a>', 
                          DI, 
                          Titles),
         Titles = lapply(Titles, gt::html)) %>% #Hyperlink each Title with DOI link
  arrange(factor(name, levels = c("Flesch",
                                  "Flesch.Kincaid",
                                  "FOG",
                                  "SMOG",
                                  "ARI",
                                  "Coleman.Liau.grade")), value) %>%
  group_by(name) %>% #while we could use 
  #`group_by(name) %>% arrange(name, value)`
  #using `arrange(factor())` allows us to
  #set the order of the groups!
  filter((name %in% c("Flesch", "Flesch.Kincaid", "FOG", "SMOG", "ARI", "Coleman.Liau.grade") & 
            (name %in% "Flesch" & row_number() %in% 1:10) |
            (row_number() %in% (n()-9):n() & row_number() %in% 1:10))) %>% 
  #above we "filtered" the first 10 and bottom 10 rows of corresponding stats
  #remember that we have "arranged" each group by ascending values 
  arrange(value = ifelse(name %in% "Flesch", desc(value),
                         value)) %>% #if the name column contains Flesch
  #"arrange" by decreasing values 
  gt() %>%
  cols_hide(columns = c(document, DI)) %>%
  tab_stubhead(label = "Titles") %>%
  fmt_number(columns = value, decimals = 2)  %>%
  tab_header(
    title = md("The *least* readable documents"),
    subtitle = "Six readability statistics; Flesch, Flesch.Kincaid, FOG, SMOG, ARI, Coleman.Liau.grade"
  ) %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", 
                size = 24)
    )
  )%>%
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    row_group.background.color = "grey") %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", 
                   weight = px(3)),
      cell_text(weight = "bold")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(
        align = "center",
        weight = "bold"
      )
    ),
    locations = list(
      cells_row_groups(
        groups = c("Flesch",
                   "Flesch.Kincaid",
                   "FOG",
                   "SMOG",
                   "ARI",
                   "Coleman.Liau.grade")
      )
    )
  ) %>%
  cols_align(
    align = "left",
    columns = Titles
  ) %>%
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = Titles
    )
  ) %>%
  tab_source_note(
    source_note = md("Source: SCOPUS, Colorectal Metagenome and Metabolome Search")
  ) %>%
  tab_source_note(
    source_note = md(
      'Query: TITLE-ABS-KEY ( "colorectal cancer*"  OR  "colorectal neoplas*"  OR  "adenomatous polyposis coli"  OR  "colon* neoplas*"  OR  "rectal neoplas*"  OR  "hereditary nonpolypo*"  AND  "metagenom*"  AND  "metabol*" )'
    )
  ) %>%
  tab_footnote(
    footnote = md("The **lowest** estimated grade level."),
    locations = cells_body(
      columns = value, 
      rows = value == min(value)
    )
  ) %>%
  opt_footnote_marks(marks = c("*", "+"))
 