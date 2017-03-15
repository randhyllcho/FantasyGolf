library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

url <- "http://www.pgatour.com/stats/stat.02568.html"
page <- read_html(url)
golfTable <- html_nodes(page, "table")
head(golfTable)
golfers <- html_table(golfTable)[[2]]
golfers <- data.frame(golfers)
str(golfers)
mean(golfers$ROUNDS)
colnames(golfers)[3] <- "PLAYER NAME"

scramUrl <- "http://www.pgatour.com/stats/stat.02569.html"
scramPage <- read_html(scramUrl)
scramTable <- html_nodes(scramPage, "table")  
scramblers <- html_table(scramTable)[[2]]

puttUrl <- "http://www.pgatour.com/stats/stat.02564.html"
puttPage <- read_html(puttUrl)
puttTable <- html_nodes(puttPage, "table")
putters <- html_table(puttTable)[[2]]
str(putters)

driveUrl <- "http://www.pgatour.com/stats/stat.02567.html"
drivePage <- read_html(driveUrl)
driverTable <- html_nodes(drivePage, "table")
drivers <- html_table(driverTable)[[2]]
str(drivers)

golfers <- golfers %>% full_join(putters, by = "PLAYER NAME")
golfers <- golfers %>% full_join(scramblers, by = "PLAYER NAME")
golfers <- golfers %>% full_join(drivers, by = "PLAYER NAME")
golfers <- golfers %>% select(`PLAYER NAME`, MEASURED.ROUNDS, TOTAL.SG.APP, `TOTAL SG:ARG`, `TOTAL SG:PUTTING`, `TOTAL SG:OTT`)
glimpse(golfers)

winner <- golfers %>% 
  mutate(averageAPP = TOTAL.SG.APP / MEASURED.ROUNDS, averageARG = `TOTAL SG:ARG` / MEASURED.ROUNDS, averagePutt = `TOTAL SG:PUTTING`/MEASURED.ROUNDS, averageDrive = `TOTAL SG:OTT`/MEASURED.ROUNDS)

head(winner %>% 
       filter(MEASURED.ROUNDS>median(MEASURED.ROUNDS)) %>% 
       group_by(`PLAYER NAME`) %>% 
       summarise(total = sum(averageAPP, averageARG, averagePutt, averageDrive)), 20) %>% 
  arrange(desc(total)) 

winner %>% 
  select(`PLAYER NAME`, averageAPP,averageARG, averagePutt, averageDrive) %>% 
  mutate(total = averageAPP + averageARG + averagePutt + averageDrive)

Projection <- winner %>% 
  select(`PLAYER NAME`, averageAPP,averageARG, averagePutt, averageDrive) %>%
  mutate(total = (averageDrive * 0.85) + (averageAPP * 2.24) + (averageARG * 1.12) + (averagePutt)) %>%
  arrange(desc(total))

Projection %>% View

ggplot(Projection, aes(averageAPP, averageDrive, size = total, col = total)) +
  geom_point(alpha = 0.5) +
  scale_radius(range = c(0,10)) +
  theme_classic()

