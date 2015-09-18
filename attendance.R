library(ggplot2)
library(dplyr)
library(rvest)
library(stringr)
library(magrittr)
library(tidyr)

# - - - - -
# GET DATA
# - - - - -

url <- "https://en.wikipedia.org/wiki/Average_attendances_of_European_football_clubs"
df <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table') %>%
  html_table() %>%
  data.frame()

# Clean avg attendance.
df$Average.Attendance[55] <- "29,946"
df$Average.Attendance[10] <- "50,757"
df$Average.Attendance[58] <- "29,164"
df$Average.Attendance <- df$Average.Attendance %>% 
  sub('*\\[.*]', '', .) %>%
  sub(',','', .) %>%
  as.numeric()

# Clean capacity.
df$Capacity %<>% sub(',','', .) %>%
  as.numeric()

# Add percentage filled. 
df$Percentage <- df$Average.Attendance / df$Capacity * 100

# Add stacked capability. 
df$diff <- df$Capacity - df$Average.Attendance

# - - - - - - -
# CAPACITY PLOT
# - - - - - - -

# Order bars in chart. 
dfc <- df[1:25,] %>%
  transform(., Club = reorder(Club, Average.Attendance))%>%
  gather(., "Type", "value", c(3,10))

# Calculate percentages. 
rperc <- paste0(as.character(format(round(dfc$Percentage[1:25],2), nsmall=2)), "%") %>% rev()

# Clean up legend labels. 
levels(dfc$Type)[levels(dfc$Type)=="diff"] <- "Extra Capacity"
levels(dfc$Type)[levels(dfc$Type)=="Average.Attendance"] <- "Avg. Attendance"

# Plot.
ggplot(data=dfc, aes(x=Club, y=value, color=Country, fill=Country, alpha=Type)) + 
  geom_bar(stat="identity", position="stack") +
  scale_alpha_manual(values=c(1, .3)) +
  theme(axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(hjust=1, size=12)) +
  ylab("Fan Attendance (and Capacity)") +
  ggtitle("Average Attendance of European \nFootball Clubs 2014-2015") +
  annotate("text", x=1:25, y=6000, label=rperc, color="white", angle=0) +
  coord_flip() 

