install.packages("naniar")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("prettydoc")
install.packages("mice")
install.packages("shape")
install.packages("jomo")
install.packages("pan")
install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules") #reguły
install.packages("VIM")
install.packages("validate")
install.packages("editrules")
install.packages("errorlocate")
library(errorlocate)
library(mice)
library(naniar)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(validate)
library(editrules)

library(readr)
supermarket_new <- read_csv("supermarket_new.csv")

n_miss(supermarket_new)
# jest 400 NA w danych

n_complete(supermarket_new)
# jest 16 600 pełnych wartości w danych

pct_miss(supermarket_new)
#procent NA = 2,35 %

is.special <- function(x){
      if (is.numeric(x)) !is.finite(x) else is.na(x)
}
sapply(supermarket_new, is.special)
#Czy dane zawierają inne wartości specjalne? Jeśli tak, zastąp je wartością NA.
for (n in colnames(supermarket_new)){
  is.na(supermarket_new[[n]]) <- is.special(supermarket_new[[n]])
}
summary(supermarket_new)

# Tworzenie zbioru reguł walidacji
rules <- validator(
  `Unit price` > 0,                              # Cena jednostkowa musi być większa od 0
  Total <= `Unit price` * Quantity + `Tax 5%`,  # Total musi być obliczone poprawnie
  Rating >= 1 & Rating <= 10                    # Rating musi być w przedziale 1-10
)

# Aplikowanie reguł do danych
validation_results <- confront(supermarket_new, rules)

# Podsumowanie wyników walidacji
summary(validation_results)

# Szczegóły wyników
print(validation_results)

czyste_dane <-
  supermarket_new %>%
  replace_errors(rules)

errors_removed(czyste_dane)

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

md.pattern(supermarket_new, rotate.names = TRUE)
#wykres brakujących danych

library(corrplot)
correlation_matrix <- cor(is.na(supermarket_new), use = "pairwise.complete.obs")
corrplot(correlation_matrix, method = "square")
#nie ma współzależności między brakami danych w kolumnach 

czyste_dane <- hotdeck(supermarket_new)
czyste_dane