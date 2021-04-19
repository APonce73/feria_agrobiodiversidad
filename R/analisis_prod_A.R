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
library(extrafont)
library(extrafont)

source("R/download_data.R")


LL <- Prod_A_2016 # Seleccionar una u otra opción
#LL <- Prod_B_2016 # Seleccionar una u otra opción
tabla1 <- LL %>% 
    select(contains("Cu_l_es")) %>% 
    drop_na(contains("apellidos")) %>% 
    rename(nombre_entrevistado = contains("apellidos")) %>% 
    filter(nombre_entrevistado != "Basura 1") %>% 
    filter(nombre_entrevistado != "Prueba") %>%
    filter(nombre_entrevistado != "Prueba offline") %>% 
    select(contains("Cu_l_es_su_g_n"), contains("Cu_l_es_su_edad")) %>% 
    rename(genero = contains("Cu_l_es_su_g_n")) %>% 
    rename(edad = contains("Cu_l_es_su_edad")) %>% 
    mutate(val = 1) %>% 
    mutate(edad = as.numeric(edad)) %>% 
    filter(genero != "otro")

tabla1.a <- tabla1 %>% 
    group_by(genero) %>% 
    summarise_all("mean") # aquí escojo "mean" para concoer la edad promedio de los visitantes o
# "sum" para conocer el total de participantes

tabla1.a

head(tabla1)

# Para 2017 Productores A
LL <- Prod_A_2017 # Seleccionar una u otra opción
#LL <- Prod_B_2017 # Seleccionar una u otra opción

tabla1 <- LL %>% 
    select(contains("Cu_l_es")) %>% 
    select(contains("Cu_l_es_su_g_n"), contains("Cu_l_es_su_edad")) %>% 
    rename(genero = contains("Cu_l_es_su_g_n")) %>% 
    rename(edad = contains("Cu_l_es_su_edad")) %>% 
    mutate(val = 1) %>% 
    mutate(edad = as.numeric(edad)) %>% 
    filter(genero != "otro")

tabla1.a <- tabla1 %>% 
    group_by(genero) %>% 
    summarise_all("sum") # aquí escojo "mean" para concoer la edad promedio de los visitantes o
# "sum" para conocer el total de participantes

tabla1.a

#Para Productores A  del 2018
LL <- Prod_A_2018 # Seleccionar una u otra opción
#LL <- Prod_B_2018 # Seleccionar una u otra opción

tabla1 <- LL %>% 
    rename(inicio = start) %>% 
    select(inicio, contains("Cu_l_es")) %>% 
    select(inicio, contains("Cu_l_es_su_g_n"), contains("Cu_l_es_su_edad")) %>% 
    rename(genero = contains("Cu_l_es_su_g_n")) %>% 
    rename(edad = contains("Cu_l_es_su_edad")) %>% 
    mutate(val = 1) %>% 
    mutate(edad = as.numeric(edad)) %>% 
    filter(genero != "otro") %>% 
    slice(9:nrow(Prod_A_2018))

tabla1.a <- tabla1 %>% 
    select(-inicio) %>% 
    group_by(genero) %>% 
    summarise_all("mean") # aquí escojo "mean" para concoer la edad promedio de los visitantes o
# "sum" para conocer el total de participantes

tabla1.a

head(tabla1)

#Productores A del 2019
LL <- Prod_A_2019 # Seleccionar una u otra opción
#LL <- Prod_B_2019 # Seleccionar una u otra opción

tabla1 <- LL %>%     rename(inicio = start) %>% 
    select(inicio, contains("Cu_l_es")) %>% 
    select(inicio, contains("Cu_l_es_su_g_n"), contains("Cu_l_es_su_edad")) %>% 
    rename(genero = contains("Cu_l_es_su_g_n")) %>% 
    rename(edad = contains("Cu_l_es_su_edad")) %>% 
    mutate(val = 1) %>% 
    mutate(edad = as.numeric(edad)) %>% 
    filter(genero != "otro")

tabla1.a <- tabla1 %>% 
    select(-inicio) %>% 
    group_by(genero) %>% 
    summarise_all("mean") # aquí escojo "mean" para concoer la edad promedio de los visitantes o
# "sum" para conocer el total de participantes

tabla1.a

head(tabla1,20)

#Mujeres
DD <- 43 + 46 + 42 + 46 + 40 + 41 + 44 + 45
DD / 8

# Hombres
DD <-  53 + 54 + 51 + 53 + 45 + 47 + 49 + 49
DD / 8

# Para las especies que llevan o compran en la feria
source("R/funcion_Productores_A.R")

Prod_A_2016_1 <- Prod_A_2016 %>% 
    select(matches("_Cu_ntos_tipos_o_variedades_de") | 
               matches("Si_seleccion_otro_e_riba_otros_productos" )) %>% 
    rename(producto = "_Cu_ntos_tipos_o_variedades_de") %>% 
    rename(otros = "Si_seleccion_otro_e_riba_otros_productos") %>% 
    select(producto, otros)

Prod_A_2016_final <- funcion_prod_A(Prod_A_2016_1, 2016)

Prod_A_2017_1 <- Prod_A_2017 %>% 
    select(matches("_Cu_ntos_tipos_o_variedades_de") | 
               matches("Si_seleccion_otro_e_riba_otros_productos" )) %>% 
    rename(producto = "_Cu_ntos_tipos_o_variedades_de") %>% 
    rename(otros = "Si_seleccion_otro_e_riba_otros_productos") %>% 
    select(producto, otros)

Prod_A_2017_final <- funcion_prod_A(Prod_A_2017_1, 2017)

Prod_A_2018_1 <- Prod_A_2018 %>% 
    select(matches("Cuantos_tipos_o_variedades_de") | 
               matches("Si_seleccion_otro_e_riba_otros_productos" )) %>% 
    rename(producto = "Cuantos_tipos_o_variedades_de") %>% 
    rename(otros = "Si_seleccion_otro_e_riba_otros_productos") %>% 
    select(producto, otros)

Prod_A_2018_final <- funcion_prod_A(Prod_A_2018_1, 2018)


Prod_A_2019_1 <- Prod_A_2019 %>% 
    select(matches("Cuantos_tipos_o_variedades_de") | 
               matches("Si_seleccion_otro_e_riba_otros_productos" )) %>% 
    rename(producto = "Cuantos_tipos_o_variedades_de") %>% 
    rename(otros = "Si_seleccion_otro_e_riba_otros_productos") %>% 
    select(producto, otros)

Prod_A_2019_final <- funcion_prod_A(Prod_A_2019_1, 2019)

Prod_A_2016_final1 <- Prod_A_2016_final %>%
    select(producto, val) %>% 
    rename("2016" = "val") 
Prod_A_2017_final1 <- Prod_A_2017_final %>% 
    select(producto, val) %>% 
    rename("2017" = "val")
Prod_A_2018_final1 <- Prod_A_2018_final %>% 
    select(producto, val) %>% 
    rename("2018" = "val")
Prod_A_2019_final1 <- Prod_A_2019_final %>% 
    select(producto, val) %>% 
    rename("2019" = "val")

Prod_A_final <- Prod_A_2016_final1 %>% 
    full_join(Prod_A_2017_final1, by = "producto") %>% 
    full_join(Prod_A_2018_final1, by = "producto") %>% 
    full_join(Prod_A_2019_final1, by = "producto") %>% 
    mutate(producto = str_replace(producto, "_", " ")) %>% 
    mutate(producto = str_to_title(producto)) %>% 
    mutate(producto = str_replace(producto, "Maiz", "Maíz")) %>% 
    mutate(producto = str_replace(producto, "Dulce De_calabaza", "Dulce de Calabaza")) %>% 
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
  #  mutate(producto = str_replace(producto, "Amaranto", "Amarantos")) %>% 
    mutate(producto = str_replace(producto, "Ajonjol", "Ajonjolíes")) %>% 
    mutate(producto = str_replace(producto, "Bule", "Bules"))

#write.table(Prod_A_final, file = "datos/Tabla_Prod_A_final.txt", sep = "\t")
#Para figuras
#TablaLL <- Prod_A_final %>% 
#    select(producto, "2016") %>%
#    rename(valores = `2016`) %>%  
#    arrange(desc("valores")) %>% 
#    drop_na() %>% 
#    slice(1:15)
#

#theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))
theme_set(theme_minimal(base_size = 12, base_family = "Times New Roman"))


FiguraLL <- function(basedatos, columnax, columnay, colores1){
    TablaLL <- basedatos %>% 
        select(columnax, columnay) %>%
        rename(valores = columnay) %>%  
        arrange(desc("valores")) %>% 
        drop_na() %>% 
        slice(1:15)
    
    plot2 <- ggplot(data = TablaLL) +
        geom_col(aes(x = reorder(producto, valores), y = valores), fill = colores1) + 
        coord_flip() +
        #theme_minimal() +
        #scico::scale_color_scico(palette = "bamako", direction = -1) +
        ## custom labels
        labs(
            title = paste0(columnay),
            #    subtitle = 'A scatter plot of bill depth versus bill length.',
            #caption = 'Datos levantados con la aplicación de KoboCollect',
            x = '', 
            y = ''
        )
    
    plot2
    
}


Fig1 <- FiguraLL(Prod_A_final, "producto", "2016", "#7AA095")
Fig2 <- FiguraLL(Prod_A_final, "producto", "2017", "#40531B")
Fig3 <- FiguraLL(Prod_A_final, "producto", "2018", "#516F33")
Fig4 <- FiguraLL(Prod_A_final, "producto", "2019", "#618B4A")

gt <- ((Fig1 + Fig2) / (Fig3 + Fig4)) +
    plot_annotation(title = 'Los principales 15 productos',
                    subtitle = "de las Ferias de Agrobiodiversidad en Oaxaca") 
gt1 <- patchwork::patchworkGrob(gt)
gt2 <- gridExtra::grid.arrange(gt1, bottom = "Número de campesinos que llevaron productos")

#((Fig1 + Fig2) / (Fig3 + Fig4))
ggsave(plot = gt2 ,"figuras/Figura1.jpeg", width = 9, height = 8, device = "jpeg", dpi = 500)

Prod_A_final1 <- Prod_A_final %>% 
    select(producto, val) %>% 
    group_by(producto) %>% 
    summarise_all(sum) %>% 
    arrange(desc(val))

uno <- (nrow(Prod_A_2016_final) + nrow(Prod_A_2017_final) +
            nrow(Prod_A_2018_final) + nrow(Prod_A_2019_final))/4
uno


names(Prod_A_2016)
Nolan <- Prod_A_2016 %>% 
    select(contains("_Cu_ntos_tipos_o_variedades"))

Nolan <- Nolan[,-1] 

Nolan <- type_convert(Nolan)

round(mean(apply(Nolan, 1, sum)),0)

