library(tidyverse)
library(ggthemes)

files <- list.files("data/", pattern = "*.csv", full.names = TRUE)

data <- read_csv(files, id = "annee", skip = 1, col_names = c("id", "num_police", "cout_sin"), col_select = all_of(2:3)) |> 
  mutate(annee = paste0("Année ", parse_number(annee)))

kpi_without_zero <- data |> 
  summarise(
    nb_sin = n(),
    cout_moyen = mean(cout_sin), 
    .by = c(annee, num_police)
    )


kpi_with_zero <- expand_grid(annee = c("Année 1", "Année 2", "Année 3"), num_police = 1:100000) |> 
  left_join(kpi_without_zero) |> 
  replace_na(list(nb_sin = 0, cout_moyen = 0))


kpi_with_zero |> 
  ggplot(aes(x = nb_sin, fill = annee)) +
  scale_y_log10(breaks = scales::breaks_log(n = 10), labels = scales::label_number_auto()) + 
  geom_bar(position = "dodge", color = "darkblue", alpha = 0.7) +
  labs(title = "Distribution du nombre de sinistre",
       x = "Nombre de sinistre",
       y = "Nombre de police",
       fill = NULL) +
  theme_hc() 


kpi |>
  ggplot(aes(x = cout_moyen)) +
  facet_wrap(vars(annee), ncol = 1, scales = "free_y") +
  geom_histogram(bins = 100, fill = "skyblue", color = "darkblue", alpha = 0.7) +
  scale_x_log10(breaks = scales::breaks_log(n = 10)) +
  labs(title = "Distribution du coût moyen par année",
       x = "Coût moyen (log10)",
       y = "Fréquence") +
  theme_hc()



test2 |>
  ggplot(aes(x = cout_moyen)) +
  facet_wrap(vars(annee), ncol = 1, scales = "free_y") +
  geom_histogram(bins = 100, fill = "skyblue", color = "darkblue", alpha = 0.7) +
  scale_y_log10() +
  labs(title = "Distribution du coût moyen par année",
       x = "Coût moyen (log10)",
       y = "Fréquence") +
  theme_hc()





test2 <-test |> 
  


test <- data |> 
  summarise(sum_cout_sin = sum(cout_sin), .by = c(year, num_police)) |> 
  mutate(decoupage = cut(sum_cout_sin, breaks = 5, dig.lab = 0))

test |> 
  expand(year, num_police) |> 
  count(year)


test |> 
  ggplot() + 
  facet_wrap(vars(year), dir = "h") + 
  geom_histogram(aes(x = sum_cout_sin)) + 
  scale_x_log10() + 
  theme_hc()

test |> 
  ggplot() + 
  facet_wrap(vars(year), dir = "v") + 
  geom_col(aes(x = decoupage, y = sum_cout_sin)) + 
  theme_hc()



data |> 
  summarise(sum_count_sin = sum(count_sin), .by = num_police) |> 
  ggplot() + 
  geom_histogram(aes(x = sum_count_sin)) + 
  scale_x_log10() + 
  theme_minimal()
 

ggplot(kpi, aes(x = nb_sin, fill = annee)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Distribution des nombres de sinistres par police et par année",
       x = "Nombre de sinistres",
       y = "Fréquence") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
