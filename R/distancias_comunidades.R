library(readxl)
library(tidyverse)
library(R.utils)
library(openxlsx)
library(ggrepel)
library(ggthemes)
library(extrafont)


root <- c("datos/")
titulo1 <- c("Distancias_comunidades")
type <- c(".xlsx")
formula <- paste0(root, titulo1, type)
Distancia <- read_xlsx(formula, sheet = "Sheet2") %>% 
    filter(Cultivos == "Maíz") %>% 
    mutate(Diferencia1 = Altitud_origen - Altitud_destino)
names(Distancia)

Distancia1 <- Distancia %>% 
    filter(Diferencia1 < 0)

Distancia2 <- Distancia %>% 
    filter(Diferencia1 >= 0)

write.xlsx(Distancia, file = paste0("datos/", "Cuau_info.xlsx"))

theme_set(theme_minimal(base_size = 12, base_family = "Times New Roman"))

Fig1 <- ggplot(data = Distancia) +
    geom_point(aes(x = 0, y = Altitud_origen), size = 4, colour = "#264653") +
    geom_point(aes(x = Km_recorridos, y = Altitud_destino), size = 4, colour = "#264653") +
    geom_curve(data = Distancia1, aes(x = 0, y = Altitud_origen, xend = Km_recorridos, yend = Altitud_destino),
               color = "#5DADE2",
               curvature = -0.2, arrow = arrow(length = unit(0.01, "npc")), size = 2) +
    geom_curve(data = Distancia2, aes(x = 0, y = Altitud_origen, xend = Km_recorridos, yend = Altitud_destino),
               color = "#FF5733",
               curvature = 0.1, arrow = arrow(length = unit(0.01, "npc")), size = 2) +
    geom_text_repel(aes(x = 0, y = Altitud_origen, label = short_name_origen),
                    min.segment.length = 0, seed = 42, box.padding = 0.7) +
    geom_text_repel(aes(x = Km_recorridos, y = Altitud_destino, label = short_name_destino),
                    min.segment.length = 0, seed = 42, box.padding = 0.7,
                    nudge_x = 0.15, nudge_y = 1) +
    #theme_minimal() +
    theme(
        legend.position = "none")


Fig1 + labs(title = "Relación entre la altitud* (eje vertical) y la distancia recorrida** (eje horizontal)\n de la semilla de maíz",
            x = "", 
         y = "",
         caption = "* metros sobre el nivel del mar\n ** en kilómetros") 

ggsave("figuras/Figura7.jpeg", width = 12, height = 7, device = "jpeg")
