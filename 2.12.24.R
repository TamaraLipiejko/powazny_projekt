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
install.packages("gtsummary")
install.packages("cardx")
install.packages("corrplot")

library(corrplot)
library(cardx)
library(gtsummary)
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
  mutate(Percentage = Total_Sum / sum(Total_Sum) * 100) %>%  # Obliczenie procentów
  ggplot(aes(x = "", y = Total_Sum, fill = City, label = paste0(round(Percentage, 1), "%"))) + 
  geom_bar(stat = "identity", width = 1, show.legend = TRUE, alpha = 0.85) +
  coord_polar("y") +  # Tworzenie wykresu kołowego
  scale_fill_manual(values = c(
    "olivedrab", "palevioletred2", "moccasin", "olivedrab", "lightcoral", "navajowhite"
  )) +  # Kolory dla miast
  geom_text(position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Dodanie procentów na wykresie
  labs(
    title = "Total Sales (in thousands) by City",
    subtitle = "Comparison of total sales across cities",
    x = NULL,
    y = "Total Sales (USD)"
  ) +
  scale_y_continuous(labels = function(x) x / 1000, name = "Total Sales (in thousands)")+
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "lightcoral"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, face = "bold", color = "gray30"),
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
    "olivedrab", "palevioletred2", "moccasin", "olivedrab", "lightcoral", "navajowhite"
  )) +  # Różowe odcienie
  labs(
    title = "Quantity by City",
    subtitle = "Comparison of the number of occurrences in each city",
    x = "City",
    y = "Quantity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "lightcoral"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )


#Wykres Product line purchases by gender
czyste_dane %>%
  group_by(Gender, `Product line`) %>%
  summarise(Quantity = n()) %>%
  mutate(Percentage = Quantity / sum(Quantity) * 100) %>%  # Obliczenie procentowego udziału
  ggplot(aes(x = `Product line`, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +  # Procentowy wykres skumulowany
  scale_fill_manual(values = c("lightcoral", "steelblue2")) +  # Kolory dla płci
  labs(
    title = "Product Line Purchases by Gender",
    subtitle = "Percentage distribution of product purchases by gender",
    x = "Product Line",
    y = "Percentage",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "steelblue"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )



#Wykres Total quantity sold by product line
czyste_dane %>%
  group_by(`Product line`) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE)) %>%
  mutate(Percentage = Total_Quantity / sum(Total_Quantity) * 100, 
         label = paste0(round(Percentage, 1), "%")) %>%  # Dodanie etykiety z procentem
  ggplot(aes(x = 2, y = Percentage, fill = `Product line`)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.85) +
  coord_polar("y", start = 0) +
  xlim(0.5, 2.5) +  # Dodanie przestrzeni na "wycięcie" dla wykresu pierścieniowego
  scale_fill_manual(values = c("olivedrab", "palevioletred2", "moccasin", "lightcoral", "steelblue2", "goldenrod1")) +
  labs(
    title = "Total Quantity Sold by Product Line",
    subtitle = "Proportional representation of sales across product lines",
    fill = "Product Line"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "steelblue"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  # Dodanie tekstów z procentami obok każdego segmentu wykresu
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 5)


#Wykres przedstawiający kupowanie produktów przez klientów Normal vs Member
czyste_dane %>%
  group_by(`Customer type`) %>%
  summarise(Total = n()) %>%  # Liczba zamówień dla każdego typu klienta
  mutate(Percentage = Total / sum(Total) * 100, 
         label = paste0(round(Percentage, 1), "%")) %>%  # Dodanie etykiety z procentem
  ggplot(aes(x = 2, y = Percentage, fill = `Customer type`)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.85) +
  coord_polar("y", start = 0) +
  xlim(0.5, 2.5) +  # Dodanie przestrzeni na "wycięcie" dla wykresu pierścieniowego
  scale_fill_manual(values = c("olivedrab", "palevioletred2")) +  # Kolory dla typów klientów
  labs(
    title = "Total by Customer Type",
    subtitle = "Proportional representation of total purchases by customer type",
    fill = "Customer Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "steelblue"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  # Dodanie tekstów z procentami obok każdego segmentu wykresu
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 5)




#Wykres przedstawiający ceny za jednostkę według linii produktowej
czyste_dane %>%
  ggplot(aes(x = `Product line`, y = `Unit price`, fill = `Product line`)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.85) +  # Wykres pudełkowy
  scale_fill_manual(values = c(
    "olivedrab", "palevioletred2", "moccasin", "lightcoral", "steelblue2", "goldenrod1"
  )) +  # Kolory
  labs(
    title = "Unit Price Distribution by Product Line",
    subtitle = "Comparison of unit prices among product lines",
    x = "Product Line",
    y = "Unit Price (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "steelblue"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )


# Przetwarzanie kolumny "Date" na format daty
czyste_dane <- czyste_dane %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))  # Dopasuj format, jeśli jest inny niż %m/%d/%Y

# Sprawdzenie, czy kolumna 'Date' została poprawnie przekształcona
head(czyste_dane$Date)

#Wyres: Sprzedaż z podziałęm na sklepy na przestrzeni 3 miesięcy
ggplot(czyste_dane, aes(x = Date, y = Total, color = City)) +
  geom_line(size = 1) +  # Dodanie linii na wykresie
  labs(
    title = "Sprzedaż z podziałem na sklepy na przestrzeni 3 miesięcy",
    x = "Data",
    y = "Całkowita sprzedaż",
    color = "Miasto"
  ) +
  theme_minimal(base_size = 14) +  # Użycie minimalnej tematyki
  theme(
    plot.title = element_text(color = "darkblue", size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(color = "darkblue", size = 14),
    axis.text = element_text(color = "black", size = 12),
    legend.title = element_text(color = "darkblue", size = 12),
    legend.text = element_text(color = "darkblue", size = 12),
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "lightblue"),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Pochylenie tekstu na osi X (dat)
  ) +
  scale_x_date(
    date_labels = "%b %d",  # Format daty na osi X (np. Jan 01)
    date_breaks = "10 days"  # Słupki co 10 dni
  ) +
  facet_wrap(~City)  # Facetowanie wykresu na poszczególne miasta


# Wykres zależności liczby zakupionych produktów vs całkowitej ceny z podatkiem 

ggplot(czyste_dane, aes(x = factor(Quantity), y = Total)) +
  geom_boxplot(color = "lightgreen", fill = "darkgreen", alpha = 0.7) +  # Tworzenie boxplotu
  labs(
    title = "Wykres zależności liczby zakupionych produktów vs całkowitej ceny z podatkiem",
    x = "Liczba zakupionych produktów",
    y = "Całkowita cena z podatkiem"
  ) +
  theme_minimal() +  # Estetyczny, minimalny styl
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#Mapa ciepła: Częstotliwość występowania (Quantity vs Total)
# Przygotowanie danych do mapy ciepła
heatmap_data <- czyste_dane %>%
  mutate(Quantity = factor(Quantity)) %>%  # Konwersja Quantity na factor
  group_by(Quantity, Total_bin = cut(Total, breaks = seq(0, max(Total), by = 50))) %>%  # Binowanie Total
  summarise(Frequency = n(), .groups = "drop")  # Liczenie częstotliwości

# Tworzenie mapy ciepła
ggplot(heatmap_data, aes(x = Quantity, y = Total_bin, fill = Frequency)) +
  geom_tile(color = "white") +  # Białe linie oddzielające komórki
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Częstotliwość") +  # Gradient kolorów
  labs(
    title = "Mapa ciepła: Częstotliwość występowania (Quantity vs Total)",
    x = "Liczba zakupionych produktów",
    y = "Całkowita cena z podatkiem (przedziały)"
  ) +
  theme_minimal() +  # Minimalny styl
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Heatmap: Średnia ocena klientów według oddziału i kategorii produktów
# Grupowanie danych:
heatmap_rating <- czyste_dane %>%
  group_by(Branch, `Product line`) %>%
  summarise(Avg_Rating = mean(Rating, na.rm = TRUE), .groups = "drop")  # Średnia ocena klientów

# Tworzenie heatmapy
ggplot(heatmap_rating, aes(x = Branch, y = `Product line`, fill = Avg_Rating)) +
  geom_tile(color = "white") +  # Białe linie oddzielające komórki
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Średnia ocena") +  # Gradient od jasnej do ciemnej zieleni
  labs(
    title = "Średnia ocena klientów w zależności od oddziału i kategorii produktów",
    x = "Oddział",
    y = "Kategoria produktów"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Heatmap: Średnia ocena klientów według miasta i kategorii produktów
# Grupowanie danych:
heatmap_rating_city <- czyste_dane %>%
  group_by(City, `Product line`) %>%
  summarise(Avg_Rating = mean(Rating, na.rm = TRUE), .groups = "drop")  # Średnia ocena klientów

# Tworzenie heatmapy
ggplot(heatmap_rating_city, aes(x = City, y = `Product line`, fill = Avg_Rating)) +
  geom_tile(color = "white") +  # Białe linie oddzielające komórki
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Średnia ocena") +  # Gradient od jasnej do ciemnej zieleni
  labs(
    title = "Średnia ocena klientów w zależności od miasta i kategorii produktów",
    x = "Miasto",
    y = "Kategoria produktów"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#Statystyki, których nie wiemy czy nie wywalić :)
# Przetwarzanie kolumny "Date" na format daty
czyste_dane <- czyste_dane %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Tworzenie podsumowania statystycznego
statystyki <- czyste_dane %>%
  select(City, Gender, `Customer type`, `Product line`, Total, Rating, Quantity) %>%
  tbl_summary(
    by = Gender,  # Podział wyników wg płci
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",  # Średnia i odchylenie standardowe dla danych ciągłych
      all_categorical() ~ "{n} ({p}%)"    # Liczności i procenty dla zmiennych kategorycznych
    ),
    digits = all_continuous() ~ 2,  # Zaokrąglenie dla danych ciągłych
    label = list(
      City ~ "City",
      `Customer type` ~ "Customer Type",
      `Product line` ~ "Product Line",
      Total ~ "Total Sales",
      Rating ~ "Customer Rating",
      Quantity ~ "Quantity"
    )
  ) %>%
  add_p() %>%  # Dodanie testów istotności statystycznej
  modify_header(label ~ "**Variable**") %>%
  bold_labels()

# Wyświetlenie tabeli
statystyki

#Tabela 1. Rozkład cen jednostkowych wg linii produktów
czyste_dane %>%
  select('Unit price', 'Product line') %>%
  tbl_summary(
    by = 'Product line',
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{N_nonmiss}", "{mean}", "{sd}",
      "{median} ({p25}, {p75})",
      "{min}, {max}"
    ),
    missing = "no",
    label = 'Unit price' ~ "**Cena jednostkowa**"
  ) %>%
  modify_header(label ~ "**Zmienna**") %>%
  modify_caption("**Tabela 1. Rozkład cen jednostkowych wg linii produktów**") %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  as_gt() %>%
  gt::tab_style(
    style = list(gt::cell_text(weight = "bold")),
    locations = gt::cells_column_labels()
  )

install.packages("ggstatsplot")
library(ggstatsplot)
library(dplyr)


library(ggstatsplot)
library(dplyr)

# Rozkład cen jednostkowych w zależności od produktu
czyste_dane %>%
  ggbetweenstats(
    x = `Product line`,   
    y = `Unit price`,     
    type = "parametric",  
    pairwise.comparisons = TRUE,  
    centrality.tendency = "mean",  # Średnia jako miara tendencji centralnej
    centrality.difficulty = "median",  # Mediana jako alternatywa
    p.adjust.method = "bonferroni",  # Korekta p wartości
    ggtheme = theme_minimal(),  
    title = "Rozkład cen jednostkowych w zależności od linii produktów"  # Tytuł wykresu
  )

#Porównanie cen jednostkowych w zależności od typu klienta
czyste_dane %>%
  ggbetweenstats(
    x = `Customer type`,    
    y = `Unit price`,       
    type = "parametric",    
    pairwise.comparisons = TRUE,  
    centrality.tendency = "mean",  # Średnia
    centrality.difficulty = "median",  # Mediana
    p.adjust.method = "bonferroni",  # Korekta wartości p
    ggtheme = theme_minimal(),
    title = "Porównanie cen jednostkowych w zależności od typu klienta"  # Tytuł wykresu
  )

#Rozkłąd ilości produktów w zależności od metody płatności
czyste_dane %>%
  ggbetweenstats(
    x = `Payment`,           
    y = `Quantity`,          
    type = "parametric",     
    pairwise.comparisons = TRUE,  
    centrality.tendency = "mean",  # Średnia
    centrality.difficulty = "median",  # Mediana
    p.adjust.method = "bonferroni",  # Korekta p wartości
    ggtheme = theme_minimal(),  
    title = "Rozkład ilości produktów w zależności od metody płatności"  # Tytuł wykresu
  )


#Macierz korelacji

# Wybierz tylko zmienne liczbowe z danych
numeric_data <- czyste_dane %>%
  select('Unit price', Quantity, Total, `Tax 5%`)

# Oblicz macierz korelacji
corr_matrix <- cor(numeric_data, use = "complete.obs")

# Wizualizacja korelacji z wartościami liczbowymi
corrplot(corr_matrix, method = "number", type = "upper", diag = FALSE, 
         title = "Macierz korelacji - wartości liczbowe")

# Wizualizacja korelacji w formie kolorowego wykresu
corrplot(corr_matrix, method = "color", title = "Macierz korelacji - kolory")

