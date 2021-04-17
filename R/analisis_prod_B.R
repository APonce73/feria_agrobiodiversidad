library(readxl)
library(tidyverse)
library(R.utils)
library(openxlsx)
library(ggforce)
library(scico)
library(patchwork)
library(magick)
library(ggtext)
library(ggthemes)
library(ggstream)
library(paletteer)
library(extrafont)
library(RColorBrewer)
source("R/download_data.R")
source("R/funcion_Productores_B.R")


#Para 2016
# ¿Que semillas trajo a intercambiar?
Tabla1_2016 <- semillas_intercambiar(base_datos = Prod_B_2016, year1 = "2016")

# Adquiriste semillas en ferias pasadas?
Tabla2_2016 <- adquiriste_semillas(base_datos = Prod_B_2016, year1 = "2016")

#Las semillas que adquiriste en ferias pasadas las sembrastes?
Tabla3_2016 <-  sembraste_semillas(base_datos = Prod_B_2016, year1 = "2016")

#Las semillas que adquiriste y sembraste en ferias pasadas ¿se te dieron?
Tabla4_2016 <- crecieron_semillas(base_datos = Prod_B_2016, year1 = "2016")

#¿Continua sembrando esas semillas?
Tabla4.1_2016 <- continua_sembrando(base_datos = Prod_B_2016, year1 = "2016")

# En esta feria ¿Qué semillas has adquirido?
Tabla5_2016 <- semillas_adquiridas_ahora(base_datos =  Prod_B_2016, year1 = "2016")



#Para 2017
# Adquiriste semillas en ferias pasadas?
Tabla2_2017 <- adquiriste_semillas_1(base_datos = Prod_B_2017, year1 = "2017")

#Las semillas que adquiriste en ferias pasadas las sembrastes?
Tabla3_2017 <-  sembraste_semillas_1(base_datos = Prod_B_2017, year1 = "2017")

#Las semillas que adquiriste y sembraste en ferias pasadas ¿se te dieron?
Tabla4_2017 <- crecieron_semillas_1(base_datos = Prod_B_2017, year1 = "2017")

#¿Continua sembrando esas semillas?
Tabla4.1_2017 <- continua_sembrando_1(base_datos = Prod_B_2017, year1 = "2017")

# En esta feria ¿Qué semillas has adquirido?
Tabla5_2017 <- semillas_adquiridas_ahora_1(base_datos =  Prod_B_2017, year1 = "2017")


# Para el 2018
# Adquiriste semillas en ferias pasadas?
Tabla2_2018 <- adquiriste_semillas_1(base_datos = Prod_B_2018, year1 = "2018")

#Las semillas que adquiriste en ferias pasadas las sembrastes?
Tabla3_2018 <-  sembraste_semillas_1(base_datos = Prod_B_2018, year1 = "2018")

#Las semillas que adquiriste y sembraste en ferias pasadas ¿se te dieron?
Tabla4_2018 <- crecieron_semillas_1(base_datos = Prod_B_2018, year1 = "2018")

#¿Continua sembrando esas semillas?
Tabla4.1_2018 <- continua_sembrando_1(base_datos = Prod_B_2018, year1 = "2018")

# En esta feria ¿Qué semillas has adquirido?
Tabla5_2018 <- semillas_adquiridas_ahora_1(base_datos =  Prod_B_2018, year1 = "2018")


# Para el 2019
# Adquiriste semillas en ferias pasadas?
Tabla2_2019 <- adquiriste_semillas_1(base_datos = Prod_B_2019, year1 = "2019")

#Las semillas que adquiriste en ferias pasadas las sembrastes?
Tabla3_2019 <-  sembraste_semillas_1(base_datos = Prod_B_2019, year1 = "2019")

#Las semillas que adquiriste y sembraste en ferias pasadas ¿se te dieron?
Tabla4_2019 <- crecieron_semillas_1(base_datos = Prod_B_2019, year1 = "2019")

#¿Continua sembrando esas semillas?
Tabla4.1_2019 <- continua_sembrando_1(base_datos = Prod_B_2019, year1 = "2019")

# En esta feria ¿Qué semillas has adquirido?
Tabla5_2019 <- semillas_adquiridas_ahora_1(base_datos =  Prod_B_2019, year1 = "2019")


Tabla2_F <- Tabla2_2016 %>% 
    bind_rows(Tabla2_2017) %>% 
    bind_rows(Tabla2_2018) %>% 
    bind_rows(Tabla2_2019) %>% 
    select(-"year") %>% 
    mutate(producto = as.factor(producto)) %>% 
    group_by(producto) %>%
    summarise_all(list(mean)) %>% 
    mutate(val = round(val, 0)) %>% 
    filter(val >= 1) %>%
    arrange(desc(val)) %>% 
    mutate(variable = 1) %>% 
    mutate(variable1 = "Adquiridas")

Tabla3_F <- Tabla3_2016 %>% 
    bind_rows(Tabla3_2017) %>% 
    bind_rows(Tabla3_2018) %>% 
    bind_rows(Tabla3_2019) %>% 
    select(-"year") %>% 
    mutate(producto = as.factor(producto)) %>% 
    group_by(producto) %>%
    summarise_all(list(mean)) %>% 
    mutate(val = round(val, 0)) %>% 
    filter(val >= 1) %>%
    arrange(desc(val)) %>% 
    mutate(variable = 2) %>% 
    mutate(variable1 = "Sembradas")

Tabla4_F <- Tabla4_2016 %>% 
    bind_rows(Tabla4_2017) %>% 
    bind_rows(Tabla4_2018) %>% 
    bind_rows(Tabla4_2019) %>% 
    select(-"year") %>% 
    mutate(producto = as.factor(producto)) %>% 
    group_by(producto) %>%
    summarise_all(list(mean)) %>% 
    mutate(val = round(val, 0)) %>% 
    filter(val > 2) %>% 
    arrange(desc(val)) %>% 
    mutate(variable = 3) %>% 
    mutate(variable1 = "Crecieron")

Tabla4.1_F <- Tabla4.1_2016 %>% 
    bind_rows(Tabla4.1_2017) %>% 
    bind_rows(Tabla4.1_2018) %>% 
    bind_rows(Tabla4.1_2019) %>% 
    select(-"year") %>% 
    mutate(producto = as.factor(producto)) %>% 
    group_by(producto) %>%
    summarise_all(list(mean)) %>% 
    mutate(val = round(val, 0)) %>% 
    filter(val >= 1) %>%
    arrange(desc(val)) %>% 
    mutate(variable = 4) %>% 
    mutate(variable1 = "Resiembra")


Tabla3_F1 <- Tabla2_F %>% 
    full_join(Tabla3_F, by = 'producto') %>% 
    replace_na(list(val.y = 0)) %>% 
    replace_na(list(variable.y = 2)) %>% 
    replace_na(list(variable1.y = "Sembradas")) %>% 
    select(producto, val.y, variable.y, variable1.y) %>% 
    rename(val = val.y) %>% 
    rename(variable = variable.y) %>% 
    rename(variable1 = variable1.y)


Tabla4_F1 <- Tabla2_F %>% 
    full_join(Tabla4_F, by = 'producto') %>% 
    replace_na(list(val.y = 0)) %>% 
    replace_na(list(variable.y = 3)) %>% 
    replace_na(list(variable1.y = "Crecieron")) %>% 
    select(producto, val.y, variable.y, variable1.y) %>% 
    rename(val = val.y) %>% 
    rename(variable = variable.y) %>% 
    rename(variable1 = variable1.y)

Tabla4.1_F1 <- Tabla2_F %>% 
    full_join(Tabla4.1_F, by = 'producto') %>% 
    replace_na(list(val.y = 0)) %>% 
    replace_na(list(variable.y = 4)) %>% 
    replace_na(list(variable1.y = "Resiembra")) %>% 
    select(producto, val.y, variable.y, variable1.y) %>% 
    rename(val = val.y) %>% 
    rename(variable = variable.y) %>% 
    rename(variable1 = variable1.y)



intercambios <- Tabla2_F %>% 
    bind_rows(Tabla3_F) %>% 
    bind_rows(Tabla4_F) %>% 
    bind_rows(Tabla4.1_F) %>% 
 #   bind_rows(Tabla3_F1) %>% 
 #   bind_rows(Tabla4_F1) %>% 
 #   bind_rows(Tabla4.1_F1) %>% 
    droplevels() %>% 
    mutate(colores1 = producto) %>% 
    mutate(colores1 = str_replace(colores1, "maíz", "red")) %>% 
    mutate(colores1 = str_replace(colores1, "chile", "black")) %>% 
    mutate(colores1 = str_replace(colores1, "jamaica", "yellow")) %>% 
    mutate(colores1 = str_replace(colores1, "calabaza", "green")) %>% 
    mutate(colores1 = str_replace(colores1, "chayote", "white")) %>% 
    mutate(colores1 = str_replace(colores1, "cacahuate", "orange1")) %>% 
    mutate(colores1 = str_replace(colores1, "frijol", "cyan1")) %>% 
    mutate(colores1 = str_replace(colores1, "amaranto", "brown")) %>%  
     #      colores1 = str_replace(producto, "chile", "coral3"),
     #      colores1 = str_replace(producto, "maíz", "black"),
     #      colores1 = str_replace(producto, "calabaza", "yellow"),
     #      colores1 = str_replace(producto, "chayote", "deepskyblue"),
     #      colores1 = str_replace(producto, "cacahuate", "gray"),
     #      colores1 = str_replace(producto, "frijol", "cyan2"),
     #      colores1 = str_replace(producto, "amaranto", "cyan1")) %>% 
    mutate(colores1 = as.factor(colores1)) %>% 
    arrange(variable) %>% 
    data.frame()


intercambios

#data("blockbusters")    

#, fill = producto, label = producto, color = colores

theme_set(theme_minimal(base_size = 12, base_family = "Times New Roman"))

#ggplot(data = intercambios, aes(x = variable, y = val, fill = producto, 
#                                color = producto)) +
#    geom_vline(xintercept = 1.5, linetype = "dotted") +
#    geom_vline(xintercept = 2.5, linetype = "dotted") +
#    geom_vline(xintercept = 3.5, linetype = "dotted") +
#    geom_stream(extra_span = 1.1, type = "ridge", n_grid = 3000, bw = 0.88) +
#    #geom_stream_label(size = 5, type = "ridge", n_grid = 8) +
#    #cowplot::theme_minimal_vgrid(font_size = 18) +
#    theme_void() +
#    theme(legend.position = "none",
#          axis.title = element_blank(),
#        #  axis.text.x = element_blank(),
#          axis.text.y = element_blank()) +
#    scale_colour_manual(values = c(paletteer::paletteer_d("wesanderson::IsleofDogs2"), paletteer::paletteer_d("wesanderson::IsleofDogs1")) %>% colorspace::darken(0.5)) +
#    scale_fill_manual(values = c(paletteer::paletteer_d("wesanderson::IsleofDogs2"), paletteer::paletteer_d("wesanderson::IsleofDogs1")) %>% colorspace::lighten(0.2)) +
#    labs(title = "b)",
#         #title = "Dinámica de intercambios, siembra, crecimiento y resiembra \n de las semillas adquiridas en las ferias (2016 - 2019)",
#         x = NULL,
#         y = NULL) +
#    ylim(0, 450) +
#    geom_text(aes(x = 3.7, y = 28, label = "Maíz"), hjust = 0, vjust = 0, 
#               lineheight = 0.8, colour = "#555555",  
#               family = "Helvetica", size = 4) +
#    geom_text(aes(x = 3.7, y = 110, label = "Frijol"), hjust = 0, vjust = 0, 
#               lineheight = 0.8, colour = "#555555", 
#               family = "Helvetica", size = 4) +
#    geom_label(aes(x = 2.7, y = 250, label = "Cacahuate"), hjust = 0, vjust = -0.4, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4) +
#    geom_curve(aes(x = 2, y = 170, xend = 2.7, yend = 260),
#               curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
#               colour = "tomato4", size = 0.4) +
#    geom_label(aes(x = 3.7, y = 250, label = "Calabaza"), hjust = 0, vjust = -0.5, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4) +
#    geom_curve(aes(x = 3.55, y = 88, xend = 3.7, yend = 260),
#               curvature = -0.5, arrow = arrow(length = unit(0.01, "npc")), 
#               colour = "tomato4", size = 0.4) +
#    geom_text(aes(x = 3.7, y = 70, label = "Amaranto"), hjust = 0, vjust = 0, 
#                colour = "black", 
#               family = "Helvetica", size = 4) +
#    geom_text(aes(x = 2, y = 28, label = "Chile"), hjust = -0.2, vjust = 0.5, 
#              lineheight = 0.8, colour = "#555555",  
#              family = "Helvetica", size = 4) +
#    geom_curve(aes(x = 1.3, y = 110, xend = 2, yend = 28),
#               curvature = 0.3, arrow = arrow(length = unit(0.01, "npc")), 
#               colour = "tomato4", size = 0.4) +
#    geom_label(aes(x = 2, y = 270, label = "Chayote"), hjust = 0, vjust = 0.5, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4) +
#    geom_curve(aes(x = 1.5, y = 150, xend = 2, yend = 270),
#               curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
#               colour = "tomato4", size = 0.4) +
#    geom_label(aes(x = 2, y = 300, label = "Jamaica"), hjust = 0, vjust = 0.5, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4) +
#    geom_curve(aes(x = 1.25, y = 165, xend = 2, yend = 300),
#               curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
#               colour = "tomato4", size = 0.4) +
#    geom_label(aes(x = 1.2, y = 400, label = "Semillas o productos \n adquiridos en ferias pasadas"), hjust = 0.5, vjust = 0.5, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4.5) +
#    geom_label(aes(x = 2, y = 400, label = "Semillas o productos \n sembrados"), hjust = 0.5, vjust = 0.5, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4.5) +
#    geom_label(aes(x = 3, y = 400, label = "Semillas o productos \n que crecieron"), hjust = 0.5, vjust = 0.5, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4.5) +
#    geom_label(aes(x = 3.8, y = 400, label = "Semillas o productos \n resembrados"), hjust = 0.5, vjust = 0.5, 
#               lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
#               family = "Helvetica", size = 4.5) 

#ggsave("figuras/Figura6.jpeg", width = 15, height = 8, device = "jpeg")

    #scale_colour_manual(values = intercambios$colores1 %>% colorspace::darken(.8)) +
    #scale_fill_manual(values = intercambios$colores1 %>% colorspace::lighten(.2)) 

intercambios

Tabla5_F <- Tabla5_2016 %>% 
    bind_rows(Tabla5_2017) %>% 
    bind_rows(Tabla5_2018) %>% 
    bind_rows(Tabla5_2019) %>% 
    select(-"year") %>% 
    mutate(producto = as.factor(producto)) %>% 
    group_by(producto) %>%
    summarise_all(list(mean)) %>% 
    mutate(val = round(val, 0)) %>% 
    #filter(val > 2) %>%
    arrange(desc(val)) %>% 
    #mutate(variable = 4) %>% 
    mutate(variable1 = "Adquisiciones") %>% 
    rename(Adquisiciones = val) %>% 
    select(-variable1) %>% 
    mutate(producto = str_to_title(producto)) %>% 
    mutate(producto = str_replace(producto, "Artesanias", "Artesanías")) %>% 
    mutate(producto = str_replace(producto, "Maíz", "Maíces")) %>%
    mutate(producto = str_replace(producto, "Frijol", "Frijoles")) %>%
    mutate(producto = str_replace(producto, "Calabaza", "Calabazas")) %>%
    mutate(producto = str_replace(producto, "Chayote", "Chayotes")) %>%
    mutate(producto = str_replace(producto, "Chile", "Chiles")) %>%
    mutate(producto = str_replace(producto, "Miltomate", "Miltomates")) %>%
    mutate(producto = str_replace(producto, "Amaranto", "Amarantos")) %>%
    mutate(producto = str_replace(producto, "Haba", "Habas")) %>% 
    mutate(producto = str_replace(producto, "Ejote", "Ejotes")) %>% 
    mutate(producto = str_replace(producto, "Limón", "Limones")) %>% 
    mutate(producto = str_replace(producto, "Platano", "Plátano")) %>% 
    mutate(producto = str_replace(producto, "Plátano", "Plátanos")) %>% 
    mutate(producto = str_replace(producto, "Chilacayota", "Chilacayotas")) %>% 
    mutate(producto = str_replace(producto, "Mandarina", "Mandarinas")) %>% 
    mutate(producto = str_replace(producto, "Jamaica", "Jamaicas")) %>% 
    mutate(producto = str_replace(producto, "Cacahuate", "Cacahuates")) %>% 
    mutate(producto = str_replace(producto, "Aguacate", "Aguacates")) %>%
    mutate(producto = str_replace(producto, "Tejocote", "Tejocotes")) %>% 
    mutate(producto = str_replace(producto, "Lima", "Limas")) %>% 
    mutate(producto = str_replace(producto, "Granada", "Granadas")) %>% 
    mutate(producto = str_replace(producto, "Camote", "Camotes")) %>% 
    mutate(producto = str_replace(producto, "Bule", "Bules")) %>% 
    mutate(producto = str_replace(producto, "Cacao", "Cacaos")) %>% 
    mutate(producto = str_replace(producto, "Granadas De_moco", "Granadas")) %>% 
    mutate(producto = str_replace(producto, "Ajonjol", "Ajonjolies"))




Prod_A_finalL <- Prod_A_final %>% 
    gather(.,
           `2016`,
           `2017`,
           `2018`,
           `2019`,
           key = "year",
           value = "val") %>%
    replace_na(list(val = 0)) %>%
    mutate(producto = str_replace(producto, "Algodon", "Algodón")) %>%
    mutate(producto = str_replace(producto, "Orquideas", "Orquídeas")) %>%
    mutate(producto = str_replace(producto, "Platano", "Plátanos")) %>%
    mutate(producto = str_replace(producto, "Sandia", "Sandías")) %>%
    mutate(producto = str_replace(producto, "Jicama", "Jícamas")) %>%
    mutate(producto = str_replace(producto, "Artesanias", "Artesanías")) %>%
    mutate(producto = str_replace(producto, "Cafe Pergamino", "Café")) %>%
    mutate(producto = str_replace(producto, "Cafe Pluma", "Café")) %>%
    mutate(producto = str_replace(producto, "Cafe", "Café")) %>% 
    mutate(producto = str_replace(producto, "Teplantas Medicinalestiles", "Plantas Medicinales")) %>% 
    mutate(producto = str_replace(producto, "Granadas De_moco", "Granadas")) %>% 
    select(-year)  %>% 
    group_by(producto) %>% 
    summarise_all(list(mean))  %>% 
    mutate(val = round(val, 0)) %>% 
    replace_na(list(val = 0)) %>% 
    #filter(val > 2) %>%
    arrange(desc(val)) %>% 
    #mutate(variable = 4) %>% 
    mutate(variable1 = "Produc_traidos") %>% 
    rename(Produc_traido = val) %>% 
    select(-variable1) %>% 
    full_join(Tabla5_F, by = "producto") %>% 
    replace_na(list(Adquisiciones = 0)) %>% 
    filter(Produc_traido > 3)


Prod_A_finalL_a <- Prod_A_final %>% 
    gather(.,
           `2016`,
           `2017`,
           `2018`,
           `2019`,
           key = "year",
           value = "val") %>%
    replace_na(list(val = 0)) %>%
    mutate(producto = str_replace(producto, "Algodon", "Algodón")) %>%
    mutate(producto = str_replace(producto, "Orquideas", "Orquídeas")) %>%
    mutate(producto = str_replace(producto, "Platano", "Plátano")) %>%
    mutate(producto = str_replace(producto, "Sandia", "Sandía")) %>%
    mutate(producto = str_replace(producto, "Jicama", "Jícama")) %>%
    mutate(producto = str_replace(producto, "Artesanias", "Artesanías")) %>%
    mutate(producto = str_replace(producto, "Cafe Pergamino", "Café")) %>%
    mutate(producto = str_replace(producto, "Cafe Pluma", "Café")) %>%
    mutate(producto = str_replace(producto, "Cafe", "Café")) %>% 
    mutate(producto = str_replace(producto, "Teplantas Medicinalestiles", "Plantas Medicinales")) %>% 
    mutate(producto = str_replace(producto, "Granada De_moco", "Granada")) %>% 
    select(-year)  %>% 
    group_by(producto) %>% 
    summarise_all(list(mean))  %>% 
    mutate(val = round(val, 0)) %>% 
    replace_na(list(val = 0)) %>% 
    #filter(val > 2) %>%
    arrange(desc(val)) %>% 
    #mutate(variable = 4) %>% 
    mutate(variable1 = "Produc_traidos") %>% 
    rename(Produc_traido = val) %>% 
    select(-variable1)

write.xlsx(Prod_A_finalL_a, file = "datos/Tabla_Prod_A_final.xlsx")


plot2 <- ggplot(data = Prod_A_finalL) +
    geom_col(aes(x = reorder(producto, Produc_traido), y = Produc_traido), fill = "steelblue") + 
    coord_flip() +
    #theme_tufte() +
    theme_minimal(base_size = 12, base_family = "Times New Roman",
                base_line_size = 6/22) +
    #scico::scale_color_scico(palette = "bamako", direction = -1) +
    ## custom labels
    labs(
        title = paste0("¿Qué semillas, plantas y productos\ntrajo a intercambiar a la feria?"),
        #    subtitle = 'A scatter plot of bill depth versus bill length.',
        #caption = 'Datos levantados con la aplicación de KoboCollect',
        x = '', 
        y = ''
    ) +
    ylim(0, max(Prod_A_finalL$Produc_traido)) +
    scale_y_continuous(trans = "reverse") +
    theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(hjust = 0.1, size = 12),
        axis.ticks = element_blank()
    )
    
plot2


plot3 <- ggplot(data = Prod_A_finalL) +
    geom_col(aes(x = reorder(producto, Produc_traido), y = Adquisiciones), fill = "tomato") + 
    coord_flip() +
    theme_minimal(base_line_size = 6/22) +
    ylim(0, max(Prod_A_finalL$Produc_traido)) +
   #theme_tufte() +
    #scico::scale_color_scico(palette = "bamako", direction = -1) +
    ## custom labels
    labs(
        title = paste0("¿Qué semillas, plantas y productos\n ha adquirido en la feria?"),
        #    subtitle = 'A scatter plot of bill depth versus bill length.',
        #caption = 'Datos levantados con la aplicación de KoboCollect',
        x = '', 
        y = ''
    ) +
    theme(
        axis.text.y = element_text(hjust = 0.1, size = 12),
        axis.text.x = element_text(hjust = 0.1, size = 12),
        axis.ticks = element_blank()
    )
plot3

plot2 + plot3


ggsave("figuras/Figura2.jpeg", width = 10, height = 9, device = "jpeg")


#########

dim(Prod_B_2016h)
head(Prod_B_2016h)

Adquirio <- Tabla2_F %>% 
    select(-c(variable, variable1)) %>% 
    rename(Adquirio = val)

Sembro <- Tabla3_F %>% 
    select(-c(variable, variable1)) %>% 
    rename(Sembro = val)

Crecio <- Tabla4_F %>% 
    select(-c(variable, variable1)) %>% 
    rename(Crecio = val)

Resiembra <- Tabla4.1_F %>% 
    select(-c(variable, variable1)) %>% 
    rename(Resiembra = val)

FeriasPasadas <- Adquirio %>% 
    full_join(Sembro, by = "producto") %>% 
    full_join(Crecio, by = "producto") %>% 
    full_join(Resiembra, by = "producto") %>% 
    replace_na(list(Resiembra = 0)) %>% 
    mutate(Crecio = Crecio %>% 
               is.na %>% 
               ifelse(Resiembra, Crecio)) %>% 
    mutate(Sembro = Sembro %>% 
               is.na %>% 
               ifelse(Crecio, Sembro)) %>% 
    mutate(Adquirio = Adquirio %>% 
               is.na %>% 
               ifelse(Sembro, Adquirio)) %>% 
    filter(producto != "totopo") %>% 
    filter(producto != "artesanias") %>% 
    filter(producto != "flores") %>% 
    filter(producto != "atole") %>% 
    mutate(producto = str_replace(producto, "cafetal", "cafe")) %>% 
    mutate(producto = str_replace(producto, "bandeño", "maíz")) %>% 
    mutate(producto = str_replace(producto, "calazaba", "calabaza")) %>% 
    group_by(producto) %>% 
    summarise_all(sum) %>%
    arrange(desc(Adquirio)) %>% 
    slice(1:25) %>% 
    mutate(producto = str_to_title(producto)) %>% 
    mutate(producto = str_replace(producto, "Maíz", "Maíces")) %>%
    mutate(producto = str_replace(producto, "Frijol", "Frijoles")) %>%
    mutate(producto = str_replace(producto, "Amaranto", "Amarantos")) %>%
    mutate(producto = str_replace(producto, "Calabaza", "Calabazas")) %>%
    mutate(producto = str_replace(producto, "Chayote", "Chayotes")) %>%
    mutate(producto = str_replace(producto, "Chile", "Chiles")) %>%
    mutate(producto = str_replace(producto, "Cacahuate", "Cacahuates")) %>% 
    mutate(producto = str_replace(producto, "Jamaica", "Jamaicas")) %>% 
    mutate(producto = str_replace(producto, "Bules", "Bules")) %>% 
    mutate(producto = str_replace(producto, "Cacao", "Cacaos")) %>% 
    mutate(producto = str_replace(producto, "Chilacayota", "Chilacayotas")) %>%
    mutate(producto = str_replace(producto, "Lima", "limas")) %>%
    mutate(producto = str_replace(producto, "Maracuya", "Maracuyas")) %>%
    mutate(producto = str_replace(producto, "Aguacate", "Aguacates")) %>%
    mutate(producto = str_replace(producto, "Ajonjolí", "Ajonjolíes")) %>% 
    mutate(producto = str_replace(producto, "Ajo", "Ajos")) %>% 
    mutate(producto = str_replace(producto, "Arroz", "Arroces")) %>% 
    mutate(producto = str_replace(producto, "Plátano", "Plátanos")) %>% 
    mutate(producto = str_replace(producto, "Caña", "Cañas")) %>% 
    mutate(producto = str_replace(producto, "Cebollín", "Cebollines")) %>% 
    mutate(producto = str_replace(producto, "Carambola", "Carambolas")) 
    

HH <- max(FeriasPasadas$Adquirio)
#replace_na(list(Resiembra = Crecio))

plotF1 <-  ggplot(data = FeriasPasadas) +
    geom_col(aes(x = reorder(producto, Resiembra), y = Adquirio), fill = "#610019") + 
    coord_flip() +
    ylim(0, HH) +
    #theme_minimal() +
    #scico::scale_color_scico(palette = "bamako", direction = -1) +
    ## custom labels
    labs(
        title = paste0("1. ¿Adquirió semillas\n en ferias pasadas?"),
        #    subtitle = 'A scatter plot of bill depth versus bill length.',
        #caption = 'Datos levantados con la aplicación de KoboCollect',
        x = '', 
        y = ''
    )
plotF1


plotF2 <-  ggplot(data = FeriasPasadas) +
    geom_col(aes(x = reorder(producto, Resiembra), y = Sembro), fill = "#CE4257") + 
    coord_flip() +
    ylim(0, HH) +
    theme(axis.text.y = element_blank()) +
    #theme_minimal() +
    #scico::scale_color_scico(palette = "bamako", direction = -1) +
    ## custom labels
    labs(
        title = paste0("2. ¿Las semillas adquiridas\n las sembró?"),
        #    subtitle = 'A scatter plot of bill depth versus bill length.',
        #caption = 'Datos levantados con la aplicación de KoboCollect',
        x = '', 
        y = ''
    )
plotF2

plotF3 <-  ggplot(data = FeriasPasadas) +
    geom_col(aes(x = reorder(producto, Resiembra), y = Crecio), fill = "#E76154") + 
    coord_flip() +
    ylim(0, HH) +
    theme(axis.text.y = element_blank()) +
    #theme_minimal() +
    #scico::scale_color_scico(palette = "bamako", direction = -1) +
    ## custom labels
    labs(
        title = paste0("3. ¿Las semillas sembradas\n se le dieron?"),
        #    subtitle = 'A scatter plot of bill depth versus bill length.',
        #caption = 'Datos levantados con la aplicación de KoboCollect',
        x = '', 
        y = ''
    )
plotF3

plotF4 <-  ggplot(data = FeriasPasadas) +
    geom_col(aes(x = reorder(producto, Resiembra), y = Resiembra), fill = "#FF7F51") + 
    coord_flip() +
    ylim(0, HH) +
    theme(axis.text.y = element_blank()) +
    #theme_minimal() +
    #scico::scale_color_scico(palette = "bamako", direction = -1) +
    ## custom labels
    labs(
        title = paste0("4. ¿Continua sembrando\n esas semilla?"),
        #    subtitle = 'A scatter plot of bill depth versus bill length.',
        #caption = 'Datos levantados con la aplicación de KoboCollect',
        x = '', 
        y = ''
    )
plotF4

plotF1 + plotF2 + plotF3 + plotF4 +
    plot_layout(ncol = 4)

ggsave("figuras/Figura3.jpeg", width = 11, height = 6.5, device = "jpeg")

#library(ggthemes)
#library(ggrepel)
#
#FeriasPasadas1 <- FeriasPasadas %>%
#    #mutate_at(c("Adquirio", "Sembro", "Crecio", "Resiembra"), (1)) %>% 
#    mutate(producto = str_replace_all(producto, " ", "_")) %>% 
#    slice(1:8) %>% 
#    column_to_rownames(var = "producto") %>% 
#    t() %>% 
#    as.data.frame() %>% 
#    rownames_to_column(var = "tipo")
#
#FeriasPasadas2 <- FeriasPasadas1 %>% 
#    pivot_longer(
#        cols = where(is.numeric),
#        names_to = "crop",
#        values_to = "intercambio"
#    ) %>% 
#    mutate(
#        tipo = str_to_title(tipo),
#        intercambio = replace_na(intercambio, 0),
#        intercambio_id = rep(1:8, n()/8),
#        xstart = 0, 
#        ystart = 0,
#        xend = rep(cos(seq(0.3141, pi - 0.3141, 0.35)), n()/8), #el valor de 0.35 lo voy cambiando de acuerdo al número de especies
#        yend = rep(sin(seq(0.3141, pi - 0.3141, 0.35)), n()/8),
#        alpha = ifelse(intercambio > 5, TRUE, FALSE)
#    ) %>% 
#    group_by(tipo) %>% 
#    mutate(
#        primary_crop = ifelse(which.max(intercambio) == intercambio_id, TRUE, FALSE),
#        primary_crop = ifelse(intercambio == 0, FALSE, primary_crop)
#    )
#
#
#figuraDD <- function(base_datos, filtro, tituloLL) {
#    kenyan_crops4 <-
#        base_datos %>%
#        filter(tipo == filtro) %>%
#        ggplot() +
#        geom_segment(
#            aes(
#                x = 0,
#                xend = 0,
#                y = -0.5,
#                yend = 0,
#                size = max(intercambio)
#            ),
#            color = "#af601a",
#            show.legend = FALSE
#        ) +
#        geom_diagonal(
#            aes(
#                xstart,
#                ystart,
#                xend = xend,
#                yend = yend,
#                size = 0.5
#            ),
#            color = "#28b463",
#            strength = 0.75,
#            lineend = "round",
#            #size = 0.5,
#            show.legend = FALSE,
#        ) +
#        geom_point(aes(x = xend, y = yend),
#                   #color = ifelse(FeriasPasadas2$intercambio > 0, "black", "#9d0208") ) +
#                   color = "#9d0208") +
#        geom_text(aes(
#            x = xend,
#            y = yend,
#            label = ifelse(intercambio > 1, crop, NA),
#            hjust = 1,
#            vjust = -1
#        ),
#        #color = ifelse(FeriasPasadas2$intercambio > 0, "black", "#9d0208") ) +
#        color = "#9d0208") +
#        theme_tufte() +
#        ylim(c(-0.5, 1.1)) +
#        gghighlight::gghighlight(intercambio > 1) +
#        scale_size(range = c(2.75, 10)) +
#        #facet_wrap(vars(tipo), ncol = 1) +
#        theme(
#            axis.text.y = element_blank(),
#            axis.text.x = element_blank(),
#            axis.ticks = element_blank(),
#            plot.margin = margin(10, 20, 5, 20),
#            plot.background = element_rect(fill = "#efefef", color = NA),
#            #  plot.title = element_text(family = "Yeseva One", hjust = 0.5, size = 24, margin = margin(b = 10)),
#            plot.caption = element_text(
#                size = 6,
#                hjust = 0.5,
#                color = "#af601a",
#                margin = margin(t = 10)
#            ),
#            strip.text = element_text(size = 7, margin = margin(b = 2))
#        ) +
#        labs(title = tituloLL,
#             #    subtitle = 'A scatter plot of bill depth versus bill length.',
#             #caption = 'Datos levantados con la aplicación de KoboCollect',
#             x = '',
#             y = '')
#    
#    kenyan_crops4
#    
#}
#
#fig1 <- figuraDD(FeriasPasadas2, filtro = "Adquirio", tituloLL = "Adquirio")
#fig2 <- figuraDD(FeriasPasadas2, filtro = "Sembro", tituloLL = "Sembro")
#fig3 <- figuraDD(FeriasPasadas2, filtro = "Crecio", tituloLL = "Crecio")
#fig4 <- figuraDD(FeriasPasadas2, filtro = "Resiembra", tituloLL = "Resiembra")
#
#
#fig1 + fig2 + fig4 + fig3 
#
#ggsave("figuras/Figura5.jpeg", width = 15, height = 10, device = "jpeg")
#