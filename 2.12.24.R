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

# Remove all *_imp columns
czyste_dane <- czyste_dane[, !grepl("_imp$", names(czyste_dane))]
czyste_dane


#WIZUALIZACJA DANYCH

#Wykres pokazujący w którym mieście był największy Total - cena całkowita z podatkiem
czyste_dane %>%
  group_by(City) %>%
  summarise(Total_Sum = sum(Total, na.rm = TRUE)) %>%
  arrange(desc(Total_Sum)) %>%
  ggplot(aes(x = reorder(City, Total_Sum), y = Total_Sum, fill = City)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  coord_flip() +  # Odwrócenie osi dla lepszej czytelności
  scale_fill_manual(values = c(
    "#FF99CC", "#FF6699", "#FFB6C1", "#FF85A1", "#F08080", "#FFC0CB"
  )) +  # Różowe odcienie
  labs(
    title = "Total Sales by City",
    subtitle = "Comparison of total sales across cities",
    x = "City",
    y = "Total Sales (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#FF007F"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )


#Wykres pokazujący w którym mieście największa ilość towarów została zakupiona
czyste_dane %>%
  group_by(City) %>%
  summarise(Quantity = n()) %>%  # Liczba miast w każdej grupie
  arrange(desc(Quantity)) %>%
  ggplot(aes(x = reorder(City, Quantity), y = Quantity, fill = City)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  coord_flip() +  # Odwrócenie osi dla lepszej czytelności
  scale_fill_manual(values = c(
    "#FF99CC", "#FF6699", "#FFB6C1", "#FF85A1", "#F08080", "#FFC0CB"
  )) +  # Różowe odcienie
  labs(
    title = "Quantity by City",
    subtitle = "Comparison of the number of occurrences in each city",
    x = "City",
    y = "Quantity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#FF007F"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )


#Wykres Product line purchases by gender
czyste_dane %>%
  group_by(Gender, `Product line`) %>%
  summarise(Quantity = n()) %>%  # Liczba produktów w każdej kategorii i dla każdej płci
  ggplot(aes(x = `Product line`, y = Quantity, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = TRUE, alpha = 0.85) +  # Słupki obok siebie
  scale_fill_manual(values = c("#FF99CC", "#66CCFF")) +  # Kolory dla płci
  labs(
    title = "Product Line Purchases by Gender",
    subtitle = "Comparison of product categories purchased by each gender",
    x = "Product Line",
    y = "Quantity",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#FF007F"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Obrócenie etykiet na osi X
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )


#Wykres Total quantity sold by product line
czyste_dane %>%
  group_by(`Product line`) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE)) %>%  # Sumowanie liczby sprzedanych sztuk w danej kategorii
  arrange(desc(Total_Quantity)) %>%  # Sortowanie malejąco po liczbie sprzedanych sztuk
  ggplot(aes(x = reorder(`Product line`, Total_Quantity), y = Total_Quantity, fill = `Product line`)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +  # Wykres słupkowy, bez legendy
  coord_flip() +  # Odwrócenie osi dla lepszej czytelności
  scale_fill_manual(values = c(
    "#FF99CC", "#FF6699", "#FFB6C1", "#FF85A1", "#F08080", "#FFC0CB"
  )) +  # Kolory dla kategorii
  labs(
    title = "Total Quantity Sold by Product Line",
    subtitle = "Comparison of sales volume across product categories",
    x = "Product Line",
    y = "Total Quantity Sold"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#FF007F"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )


#Wykres słupkowy przedstawiający kupowanie produktów przez klientów Normal vs Member
czyste_dane %>%
  group_by(`Customer type`) %>%
  summarise(Total = n()) %>%  
  arrange(desc(Total)) %>%
  ggplot(aes(x = reorder(`Customer type`, Total), y = Total, fill = `Customer type`)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  coord_flip() +  
  scale_fill_manual(values = c(
    "#FF99CC", "#FF6699", "#FFB6C1", "#FF85A1", "#F08080", "#FFC0CB"
  )) +  # Różowe odcienie
  labs(
    title = "Total by Customer type",
    subtitle = "Comparison of the total amount spent grouped by customer type",
    x = "Customer type",
    y = "Total"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#FF007F"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )

#Wykres przedstawiający ceny za jednostkę według linii produktowej
czyste_dane %>%
  group_by(`Product line`) %>%
  summarise(`Unit price` = n()) %>%  
  arrange(desc(`Unit price`)) %>%
  ggplot(aes(x = reorder(`Product line`, `Unit price`), y = `Unit price`, fill = `Product line`)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  coord_flip() +  
  scale_fill_manual(values = c(
    "#FF99CC", "#FF6699", "#FFB6C1", "#FF85A1", "#F08080", "#FFC0CB"
  )) +  # Różowe odcienie
  labs(
    title = "Unit price by product line",
    subtitle = "Comparison of unit prices among product lines",
    x = "Product type",
    y = "Unit price"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#FF007F"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )


