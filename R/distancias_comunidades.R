library(readxl)
library(tidyverse)
library(R.utils)
library(openxlsx)
library(ggrepel)
library(ggthemes)
library(extrafont)
library(patchwork)


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

theme_set(theme_minimal(base_size = 16, base_family = "Times New Roman"))

Fig1 <- ggplot(data = Distancia) +
    geom_point(aes(x = 0, y = Altitud_origen), size = 4, colour = "#264653") +
    geom_point(aes(x = Km_recorridos, y = Altitud_destino), size = 4, colour = "#264653") +
    geom_curve(data = Distancia1, aes(x = 0, y = Altitud_origen, xend = Km_recorridos, yend = Altitud_destino),
               color = "#5DADE2",
               curvature = -0.2, arrow = arrow(length = unit(0.01, "npc")), size = 2) +
    geom_curve(data = Distancia2, aes(x = 0, y = Altitud_origen, xend = Km_recorridos, yend = Altitud_destino),
               color = "#FF5733",
               curvature = 0.1, arrow = arrow(length = unit(0.01, "npc")), size = 2) +
    geom_text_repel(aes(x = 0, y = Altitud_origen, label = Comunidad_origen)) +
    #geom_text_repel(aes(x = Km_recorridos, y = Altitud_destino, label = Comunidad_destino)) +
    #geom_text_repel(aes(x = 0, y = Altitud_origen, label = Comunidad_origen),
    #                min.segment.length = 0, seed = 42, box.padding = 0.7,
    #                nudge_x = -2, nudge_y = -2) +
    geom_text_repel(aes(x = Km_recorridos, y = Altitud_destino, label = Comunidad_destino),
                    min.segment.length = 0, seed = 42, box.padding = -0.3,
                    nudge_x = 23, nudge_y = 4) +
    xlim(0, 500) +
    theme_minimal() +
    theme(
        legend.position = "none")


Fig1 + labs(title = "b) Relación entre la altitud (eje vertical en metros*) y la distancia recorrida (eje horizontal en kilómetros) de la semilla de maíz",
            x = "", 
         y = "",
         caption = "* metros sobre el nivel del mar") +
    theme(plot.caption = element_text(hjust =  0))

ggsave("figuras/Figura7.jpeg", width = 15, height = 13, device = "jpeg", dpi = 600)
