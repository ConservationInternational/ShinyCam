library(data.table)
library(dplyr)
marin_data_complete <- fread('~/Downloads/marin_data.csv', header = TRUE, sep = ",")

# 1 & 3
marin_data_complete %>%
  group_by(Project.ID) %>%
  count(Photo.Type.Identified.by)

# 2
marin_data_complete2 <- subset(marin_data_complete, !grepl(pattern = '.JPG', x = Photo.Type.Identified.by) & Uncertainty != 'Odocoileus hemionus' & !(Photo.Type %in% c('Blank','') ))
marin_data_complete2$Uncertainty2 <- ifelse(marin_data_complete2$Uncertainty == '', 'No Response', marin_data_complete2$Uncertainty)
marin_data_complete2$Photo.Type.Identified.by2 <- ifelse(marin_data_complete2$Photo.Type.Identified.by == '', 'ANONYMOUS', marin_data_complete2$Photo.Type.Identified.by)
my.table <- table(toupper(gsub(pattern = '^ *', replacement = '', x =marin_data_complete2$Photo.Type.Identified.by2)), marin_data_complete2$Uncertainty2)
my.table.proportions <- round(prop.table(my.table, 1)*100,2)



