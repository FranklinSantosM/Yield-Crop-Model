###Yield Crop Model

#Primero, leamos dos de los conjuntos de datos de esta semana.


library(tidyverse)

key_crop_yields <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv")
land_use <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv")

#Usaré el conjunto de datos land_use solo para encontrar los países con mayor 
#población. Creemos un vector de sus nombres.

top_countries <- land_use %>%
  janitor::clean_names() %>%
  filter(!is.na(code), entity != "World") %>%
  group_by(entity) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  slice_max(total_population_gapminder, n = 30) %>%
  pull(entity)

top_countries


#Ahora creemos una versión ordenada de los datos de rendimiento de los cultivos, 
#para los países y cultivos que me interesan.

tidy_yields <- key_crop_yields %>%
  janitor::clean_names() %>%
  pivot_longer(wheat_tonnes_per_hectare:bananas_tonnes_per_hectare,
               names_to = "crop", values_to = "yield"
  ) %>%
  mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>%
  filter(
    crop %in% c("wheat", "rice", "maize", "barley"),
    entity %in% top_countries,
    !is.na(yield)
  )

tidy_yields


#¡Esta estructura de datos es perfecta para graficar el **rendimiento del cultivo 
#a lo largo del tiempo!**
  
tidy_yields %>%
  ggplot(aes(year, yield, color = crop)) +
  geom_line(alpha = 0.7, size = 1.5) +
  geom_point() +
  facet_wrap(~entity, ncol = 5) +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  labs(x = NULL, y = "yield (tons per hectare)")

#Tenga en cuenta que no todos los países producen todos los cultivos, 
#pero que el rendimiento general de los cultivos está *aumentando*.



### Muchos modelos

#Ahora ajustemos un modelo lineal a cada combinación de cultivo y país.

library(tidymodels)

tidy_lm <- tidy_yields %>%
  nest(yields = c(year, yield)) %>%
  mutate(model = map(yields, ~ lm(yield ~ year, data = .x)))

tidy_lm


#A continuación, vamos a ordenar con tidy() esos modelos para 
#obtener los coeficientes y ajustar los valores p para múltiples 
#comparaciones mientras estamos en ello.

slopes <- tidy_lm %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) %>%
  filter(term == "year") %>%
  mutate(p.value = p.adjust(p.value))

slopes


### Explorar resultados

#Ahora podemos visualizar los resultados de este modelo, 
#que está estimando cómo están cambiando los rendimientos 
#de los cultivos en todo el mundo.

library(ggrepel)
slopes %>%
  ggplot(aes(estimate, p.value, label = entity)) +
  geom_vline(
    xintercept = 0, lty = 2,
    size = 1.5, alpha = 0.7, color = "gray50") +
  geom_point(aes(color = crop), alpha = 0.8, size = 2.5, show.legend = FALSE) +
  scale_y_log10() +
  facet_wrap(~crop) +
  geom_text_repel(size = 2.5) +
  theme_light() +
  theme(strip.text = element_text(size = 12)) +
  labs(x = "increase in tons per hectare per year")

  

                  