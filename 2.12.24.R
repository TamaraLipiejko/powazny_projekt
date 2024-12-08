install.packages("naniar")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("prettydoc")
install.packages("mice")
library(mice)
library(naniar)
library(dplyr)
library(ggplot2)

library(readr)
supermarket_new <- read_csv("supermarket_new.csv")

n_miss(supermarket_new)
# jest 400 NA w danych

n_complete(supermarket_new)
# jest 16 600 pełnych wartości w danych

pct_miss(supermarket_new)
#procent NA = 2,35 %

miss_var_summary(supermarket_new)
# tabelka pokazująca w jakich kolumnach mamy NA (gross income - 150, Rating - 150, City - 100)

supermarket_new %>% 
  group_by(Gender) %>% 
  miss_var_summary() %>%
  print(n = Inf) %>%
  filter(variable %in% c("Rating", "gross income", "City"))
#Tabelka pokazuje ile NA jest wg płci

supermarket_new %>% 
  group_by(City) %>% 
  miss_var_summary() %>%
  print(n = Inf) %>%
  filter(variable %in% c("Rating", "gross income", "City"))
#Tabelka pokazuj ile NA według miasta

supermarket_new %>% 
  miss_case_table()
#tabelka pokazująca ile brakujących obserwacji jest w wierszu

vis_miss(supermarket_new)
#mapa cieplna pokazująca gdzie są NA - chyba shadow map?

gg_miss_fct(supermarket_new, fct = Gender )
#mapa cieplna NA dla płci

gg_miss_upset(supermarket_new, 
              nsets = 3)
#wykres UpSet dla współwystępowania NA  (nsets=3, bo tylko w 3 kolumnach mamy dane na)

             
ggplot(supermarket_new, aes(x = `gross income`, y = Rating )) +
  geom_point(aes(color = City, shape = City), size = 2) +
  scale_color_manual(values = c("goldenrod","deepskyblue4","darkorange")) +
  theme_minimal()      
#wykres zależności NA między rating, gross icome i city

ggplot(supermarket_new, aes(x = `gross income`, y = Rating )) +
  geom_point(size = 2, color = "cyan4") +
  theme_minimal()
#Relacja między gross income a rating

ggplot(supermarket_new, aes(x = `gross income`, y = City )) +
  geom_point(size = 2, color = "goldenrod") +
  theme_minimal()
#relacja między City a gross income - dziwna, utworzyły się paski

ggplot(supermarket_new, aes(x = `Rating`, y = City )) +
  geom_point(size = 2, color = "darkorange") +
  theme_minimal()
#relacja na między rating a city - też paski

md.pattern(supermarket_new)
#wykres, który jeszcze nie wiemy co przedstawia

