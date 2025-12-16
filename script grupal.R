# Dependencias
library(dplyr)
library(ggplot2)

# Load data
mushrooms <- read.csv("C:/Users/gemam/OneDrive/Documentos/4º de carrera/Computacion/tfinal/mushrooms.csv")

# Procesado del dataset
pr_mushroom <- mushrooms %>% 
  select(class,odor,bruises) 
pr_mushroom <- pr_mushroom %>%
  mutate(
    class = as.factor(ifelse(class == "p", 
                             "venenoso", 
                             "comestible"))
  ) %>%
  glimpse()

pro_mushroom <- pr_mushroom %>%
  mutate(
    bruises = as.factor(ifelse(bruises == "t", 
                               "Se magulla", 
                               "No magulla"))
  )

proc_mushroom <- pro_mushroom %>%
  mutate(
    odor = as.factor(
      ifelse(odor == "a", "Almendra",
             ifelse(odor == "c", "Creosota",
                    ifelse(odor == "f", "Fétido",
                           ifelse(odor == "l", "Anís",
                                  ifelse(odor == "m", "Moho",
                                         ifelse(odor == "n", "Sin olor",
                                                ifelse(odor == "p", "Acre",
                                                       ifelse(odor == "s", "Picante",
                                                              ifelse(odor == "y", "Pescado", NA)))))))))
    )
  ) %>% 
  glimpse()

# vista de los datos  

knitr::kable(head(proc_mushroom, 8), align = 'c', caption = "Tabla 2: Vista de datos procesados")


#conteo de venenosas vs comestibles

table(proc_mushroom$class)

#tabla de venenoso vs comestible
proc_mushroom %>% 
  ggplot(aes(x=class, fill = class)) +
  geom_bar(width = 0.6, show.legend = FALSE) + 
  scale_fill_manual(values = c("comestible" = "salmon",
                               "venenoso" = "green")) +
  labs(
    title = "Frecuencia de setas comestibles vs venenosas",
    x = "Clase de seta",
    y = "Número de observaciones")
  
# Test estadisticos
## Clase vs olor
tabla_odor <- table(proc_mushroom$class, proc_mushroom$odor)
tabla_odor
chisq.test(tabla_odor)
###grafico 1
proc_mushroom %>% 
  ggplot(aes(x = odor, fill = odor)) +
  geom_bar() +
  labs(
    title = "Frecuencia de cada tipo de olor",
    x = "Olor",
    y = "Número de setas"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###grafico 2
proc_mushroom %>% 
  ggplot(aes(x = odor, fill = class)) +
  geom_bar() +
  labs(
    title = "Distribución del olor según la toxicidad",
    x = "Olor",
    y = "Número de setas",
    fill = "Clase"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "comestible" = "lavender",
      "venenoso"   = "purple"))
## Clase vs bruises
tabla_bruises <- table(proc_mushroom$class, proc_mushroom$bruises)
chisq.test(tabla_bruises)

proc_mushroom %>% 
  ggplot(aes(x = class, fill = bruises)) +
  geom_bar() +
  labs(
    title = "Magullamiento según toxicidad",
    x = "Clase de seta",
    y = "Nº de setas",
    fill = "Magullamiento"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No magulla"="lavender", "Se magulla"="purple") )

