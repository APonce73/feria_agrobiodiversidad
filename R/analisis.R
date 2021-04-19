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

source("download_data.R")


LL <- Prod_A_2016 # Seleccionar una u otra opción
LL <- Prod_B_2016 # Seleccionar una u otra opción
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
LL <- Prod_B_2017 # Seleccionar una u otra opción

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
LL <- Prod_B_2018 # Seleccionar una u otra opción

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
LL <- Prod_B_2019 # Seleccionar una u otra opción

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
source("funcion_Productores_A.R")

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
    mutate(producto = str_replace(producto, "Dulce De_calabaza", "Dulce de Calabaza"))



#Para figuras
#TablaLL <- Prod_A_final %>% 
#    select(producto, "2016") %>%
#    rename(valores = `2016`) %>%  
#    arrange(desc("valores")) %>% 
#    drop_na() %>% 
#    slice(1:15)
#

#theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))
theme_set(theme_minimal(base_size = 12))

#
#plot1 <- ggplot(data = TablaLL) +
#    geom_col(aes(x = reorder(producto, valores), y = valores), fill = "#40531B") + 
#    coord_flip() +
#    theme_classic() +
#    #scico::scale_color_scico(palette = "bamako", direction = -1) +
#    ## custom labels
#    labs(
#        title = 'Los quince principales productos de la feria del 2016',
#    #    subtitle = 'A scatter plot of bill depth versus bill length.',
#        caption = 'Datos levantados con la aplicación de KoboCollect',
#        x = '', 
#        y = 'Veces mencionados por los agricultores'
#    )
#
#plot1

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

((Fig1 + Fig2) / (Fig3 + Fig4)) +
    plot_annotation(title = 'Los principales 15 productos',
                    subtitle = "de las Ferias de Agrobiodiversidad en Oaxaca")

ggsave("figuras/Figura1.jpeg", width = 9, height = 8, device = "jpeg")

Prod_A_final1 <- Prod_A_final %>% 
    select(producto, val) %>% 
    group_by(producto) %>% 
    summarise_all(sum) %>% 
    arrange(desc(val))

uno <- (nrow(Prod_A_2016_final) + nrow(Prod_A_2017_final) +
            nrow(Prod_A_2018_final) + nrow(Prod_A_2019_final))/4
uno

# Para los intercambios las bases de Prod_B
# Que semillas trajo a intercambiar
names(Prod_B_2016)

Prod_B_2016a <- Prod_B_2016 %>% 
    select(matches("_Qu_productos_traen_a_exponer")[1]) %>% 
    rename(producto = contains("productos")) %>%  
    mutate(producto = str_to_lower(producto)) %>% 
    mutate(producto = str_replace(producto, "ma_z", "maiz")) %>%
    mutate(producto = str_replace(producto, "x_1", "amaranto")) %>% 
    mutate(producto = str_replace(producto, "option_10", "plantas_medicinales")) %>% 
    mutate(producto = str_replace(producto, "x", "chayote")) %>% 
    mutate(producto = str_replace(producto, "option_11", "tortillas")) %>% 
   # mutate(producto = str_replace(producto, "option_18", "nicuatole")) %>% 
   # mutate(producto = str_replace(producto, "x", "plantas_medicinales")) %>% 
   # mutate(producto = str_replace(producto, "algod_n", "algodon")) %>%
   # mutate(producto = str_replace(producto, "caf", "cafe")) %>%
   # mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
   # mutate(producto = str_replace(producto, "ceboll_n", "cebollin")) %>%
   # mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
   # mutate(producto = str_replace(producto, "n_spero", "nispero")) %>%
   # mutate(producto = str_replace(producto, "panela_piloncillo", "piloncillo")) %>%
   # mutate(producto = str_replace(producto, "sand_a", "sandia")) %>%
    separate_rows(producto, sep = " ") %>% 
    filter(producto != "otro") %>% 
    mutate(producto = str_to_lower(producto))

    
Prod_B_2016b <- Prod_B_2016 %>% 
    select(contains("otros_productos")) %>% 
    rename(producto = contains("otros_productos")) %>%
    drop_na() %>% 
    separate_rows(producto, sep = ",") %>% 
    separate_rows(producto, sep = " y ") %>% 
    mutate(producto = str_replace(producto, ":", "")) %>% 
    mutate(producto = str_to_lower(producto)) %>% 
    mutate(producto = str_replace_all(producto, " ", "_")) %>% 
    mutate(producto = str_replace_all(producto, "\\.", "")) %>% 
    mutate(producto = str_replace_all(producto, "__", "_"))

Prod_B_2016c <- Prod_B_2016b %>% 
    filter(grepl('^_', producto)) 

str_sub(Prod_B_2016c$producto, 1, 1) <- ""

Prod_B_2016d <- Prod_B_2016b %>% 
    filter(!grepl('^_', producto)) %>% 
    bind_rows(Prod_B_2016c) %>% 
    filter(producto != "") %>% 
    mutate(producto = str_replace_all(producto, "_", " ")) %>% 
    bind_rows(Prod_B_2016a) %>%
    mutate(producto = str_replace(producto, "á", "a")) %>% 
    mutate(producto = str_replace(producto, "é", "e")) %>% 
    mutate(producto = str_replace(producto, "í", "i")) %>% 
    mutate(producto = str_replace(producto, "ó", "o")) %>% 
    mutate(producto = str_replace(producto, "ú", "u")) %>% 
    mutate(producto = recode(producto, "cola de carballo" = "cola de caballo")) %>% 
    mutate(producto = recode(producto, "coyol coroso" = "coroso")) %>%
    mutate(producto = recode(producto, "coroso" = "corozo")) %>%
    mutate(producto = recode(producto, "habas" = "haba")) %>%
    mutate(producto = recode(producto, "ajongolin" = "ajonjoli")) %>%
    mutate(producto = recode(producto, "ags" = "NA")) %>% 
    mutate(producto = recode(producto, "mispero" = "nispero")) %>% 
    mutate(producto = recode(producto, "peino" = "pepino")) %>% 
    mutate(producto = recode(producto, "aguacate bola criollo" = "aguacate criollo")) %>% 
    mutate(producto = recode(producto, "carambolo" = "carambola")) %>% 
    mutate(producto = recode(producto, "mezcales" = "mezcal")) %>% 
    mutate(producto = recode(producto, "abas" = "haba")) %>% 
    mutate(producto = recode(producto, "limas" = "lima")) %>% 
    mutate(producto = recode(producto, "chirimollas" = "chirimoya")) %>% 
    #mutate(producto = recode(producto, "caf" = "cafe")) %>% 
    #mutate(producto = recode(producto, "caf" = "cafe")) %>% 
    mutate(val = 1) %>% 
    group_by(producto) %>% 
    summarise_all("sum") %>% 
    mutate(year = "2016") %>% 
    arrange(desc(val)) %>% 
    filter(val > 1)


# Adquirist semillas en ferias pasadas?

Prod_B_2016e <- Prod_B_2016 %>% 
    select(contains("Adquiriste_semillas_en_ferias")[1]) %>% 
    rename(producto = contains("semillas_en_ferias")) %>%  
    mutate(producto = str_to_lower(producto)) %>% 
    mutate(producto = str_replace(producto, "ma_z", "maiz")) %>%
    mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>% 
    mutate(producto = str_replace(producto, "option_11", "calabaza")) %>%
    mutate(producto = str_replace(producto, "option_12", "chayote")) %>% 
    mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>%
    mutate(producto = str_replace(producto, "option_14", "limones")) %>%
    mutate(producto = str_replace(producto, "option_15", "miltomate")) %>%
    mutate(producto = str_replace(producto, "option_16", "naranjas")) %>%
    mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
    separate_rows(producto, sep = " ") %>% 
    filter(producto != "otro") %>% 
    filter(producto != "ninguna")

Prod_B_2016f <- Prod_B_2016 %>% 
    select(contains("Si_seleccion_otro_e_i_en_ferias_pasadas")) %>% 
    rename(producto = contains("Si_seleccion_otro_e_i_en_ferias_pasadas")) %>%  
    drop_na() %>% 
    separate_rows(producto, sep = ",") %>% 
    separate_rows(producto, sep = " y ") %>% 
    mutate(producto = str_to_lower(producto))

Prod_B_2016g <- Prod_B_2016f %>% 
filter(grepl('^_', producto))

Prod_B_2016h <- Prod_B_2016g %>%
    filter(!grepl('^_', producto)) %>%
    bind_rows(Prod_B_2016f) %>%
    filter(producto != "") %>%
    mutate(producto = str_replace_all(producto, "_", " ")) %>%
    bind_rows(Prod_B_2016e) %>%
    mutate(val = 1) %>%
    group_by(producto) %>%
    summarise_all("sum") %>%
    mutate(year = "2016") %>%
    arrange(desc(val))
#    filter(val > 1)

#Las semillas que adquiriste en ferias pasadas las sembrastes?

Prod_B_2016i <- Prod_B_2016 %>% 
    select(contains("semillas_que_adquiriste_e")[1]) %>% 
    rename(producto = contains("semillas_que_adquiriste")) %>%  
    mutate(producto = str_to_lower(producto)) %>% 
        mutate(producto = str_replace(producto, "option_1", "maiz")) %>% 
    mutate(producto = str_replace(producto, "option_2", "frijol")) %>%
    mutate(producto = str_replace(producto, "option_9", "amaranto")) %>%
    mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>% 
    #mutate(producto = str_replace(producto, "option_11", "calabaza")) %>%
    mutate(producto = str_replace(producto, "option_12", "hierba santa")) %>% 
    mutate(producto = str_replace(producto, "option_13", "limones")) %>%
    mutate(producto = str_replace(producto, "option_14", "miltomate")) %>%
    mutate(producto = str_replace(producto, "option_15", "naranjas")) %>%
    mutate(producto = str_replace(producto, "option_16", "nopales")) %>%
    mutate(producto = str_replace(producto, "option_1", "maiz")) %>% 
    mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
    separate_rows(producto, sep = " ") %>% 
    filter(producto != "otro") %>% 
    filter(producto != "ninguna")

Prod_B_2016j <- Prod_B_2016 %>% 
    select(contains("Si_seleccion_otro_e_s_semillas_que_sembr")) %>% 
    rename(producto = contains("Si_seleccion_otro_e_s_semillas_que_sembr")) %>%  
    drop_na() %>% 
    separate_rows(producto, sep = ",") %>% 
    separate_rows(producto, sep = " y ") %>% 
    mutate(producto = str_to_lower(producto))

Prod_B_2016k <- Prod_B_2016j %>% 
    filter(grepl('^_', producto))

Prod_B_2016L <- Prod_B_2016k %>%
    filter(!grepl('^_', producto)) %>%
    bind_rows(Prod_B_2016j) %>% 
    filter(producto != "") %>%
    mutate(producto = str_replace_all(producto, "_", " ")) %>% 
    bind_rows(Prod_B_2016i) %>% 
    mutate(val = 1) %>%
    group_by(producto) %>%
    summarise_all("sum") %>%
    mutate(year = "2016") %>%
    filter(producto != "no sabe") %>% 
    arrange(desc(val)) 

############
#Las semillas que adquiriste y sembraste en ferias pasadas ¿se te dieron?

Prod_B_2016m <- Prod_B_2016 %>% 
    select(contains("semillas_que_adquiriste_y_")[1]) %>% 
    rename(producto = contains("semillas_que_adquiriste")) %>%  
    mutate(producto = str_to_lower(producto)) %>% 
    mutate(producto = str_replace(producto, "option_2", "frijol")) %>%
    mutate(producto = str_replace(producto, "option_9", "amaranto")) %>%
    mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>% 
    mutate(producto = str_replace(producto, "option_11", "calabaza")) %>%
    mutate(producto = str_replace(producto, "option_12", "chayote")) %>% 
    mutate(producto = str_replace(producto, "option_13", "limones")) %>%
    mutate(producto = str_replace(producto, "option_14", "miltomate")) %>%
    mutate(producto = str_replace(producto, "option_15", "naranjas")) %>%
    mutate(producto = str_replace(producto, "option_16", "nopales")) %>%
    mutate(producto = str_replace(producto, "option_1", "maiz")) %>% 
    mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
    separate_rows(producto, sep = " ") %>% 
    filter(producto != "otro") %>% 
    filter(producto != "ninguna")

Prod_B_2016n <- Prod_B_2016 %>% 
    select(contains("Si_seleccion_otro_e_las_que_se_le_dieron")) %>% 
    rename(producto = contains("Si_seleccion_otro_e_las_que_se_le_dieron")) %>%  
    drop_na() %>% 
    separate_rows(producto, sep = ",") %>% 
    separate_rows(producto, sep = " y ") %>% 
    mutate(producto = str_to_lower(producto))

Prod_B_2016p <- Prod_B_2016n %>% 
    filter(grepl('^_', producto))

Prod_B_2016q <- Prod_B_2016p %>%
    filter(!grepl('^_', producto)) %>%
    bind_rows(Prod_B_2016n) %>% 
    filter(producto != "") %>%
    mutate(producto = str_replace_all(producto, "_", " ")) %>% 
    bind_rows(Prod_B_2016m) %>% 
    mutate(val = 1) %>%
    group_by(producto) %>%
    summarise_all("sum") %>%
    mutate(year = "2016") %>%
    filter(producto != "no sabe") %>% 
    arrange(desc(val)) 
    
###########
#¿Continua sembrando esas semillas?

Prod_B_2016r <- Prod_B_2016 %>% 
    select(contains("_Continua_sembrando_esas_semil")[1]) %>%  
    rename(producto = contains("_Continua_sembrando_esas_semil")) %>%  
    mutate(producto = str_to_lower(producto)) %>% 
    mutate(producto = str_replace(producto, "x", "amaranto")) %>%
    mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
    mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
    mutate(producto = str_replace(producto, "option_12", "hierba santa")) %>%
    mutate(producto = str_replace(producto, "option_13", "limones")) %>% 
    mutate(producto = str_replace(producto, "ma_z", "maiz")) %>%
    mutate(producto = str_replace(producto, "option_14", "miltomate")) %>% 
    #mutate(producto = str_replace(producto, "option_15", "naranjas")) %>%
    mutate(producto = str_replace(producto, "option_16", "nopales")) %>%
    mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
    separate_rows(producto, sep = " ") %>% 
    filter(producto != "otro") %>% 
    filter(producto != "ninguna")

Prod_B_2016s <- Prod_B_2016 %>% 
    select(contains("seleccion_otro_e_e_contin")) %>% 
    rename(producto = contains("seleccion_otro_e_e_contin")) %>%  
    drop_na() %>% 
    separate_rows(producto, sep = ",") %>% 
    separate_rows(producto, sep = " y ") %>% 
    mutate(producto = str_to_lower(producto))

Prod_B_2016t <- Prod_B_2016s %>% 
    filter(grepl('^_', producto))

Prod_B_2016u <- Prod_B_2016t %>%
    filter(!grepl('^_', producto)) %>%
    bind_rows(Prod_B_2016s) %>% 
    filter(producto != "") %>%
    mutate(producto = str_replace_all(producto, "_", " ")) %>% 
    bind_rows(Prod_B_2016r) %>% 
    mutate(val = 1) %>%
    group_by(producto) %>%
    summarise_all("sum") %>%
    mutate(year = "2016") %>%
    filter(producto != "no sabe") %>% 
    filter(producto != "aun no lo siembra") %>% 
    arrange(desc(val)) 

#########
# En esta feria ¿Qué semillas has adquirido?

Prod_B_2016v <- Prod_B_2016 %>% 
    select(contains("En_esta_feria_Qu_semillas_ha")[1]) %>%  
    rename(producto = contains("En_esta_feria_Qu_semillas_ha")) %>% 
    mutate(producto = str_to_lower(producto)) %>% 
    #mutate(producto = str_replace(producto, "x", "amaranto")) %>%
    #mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
    mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
    mutate(producto = str_replace(producto, "option_12", "hierba santa")) %>%
    mutate(producto = str_replace(producto, "option_13", "limones")) %>% 
    mutate(producto = str_replace(producto, "ma_z", "maiz")) %>%
    mutate(producto = str_replace(producto, "option_14", "miltomate")) %>% 
    mutate(producto = str_replace(producto, "option_15", "naranjas")) %>%
    mutate(producto = str_replace(producto, "option_16", "nopales")) %>%
    mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
    separate_rows(producto, sep = " ") %>% 
    filter(producto != "otro") %>% 
    filter(producto != "ninguna")

Prod_B_2016w <- Prod_B_2016 %>% 
    select(contains("Si_seleccion_otro_e_re_de_otras")) %>% 
    rename(producto = contains("Si_seleccion_otro_e_re_de_otras"))  %>% 
    drop_na() %>% 
    separate_rows(producto, sep = ", ") %>% 
    separate_rows(producto, sep = ",") %>% 
    separate_rows(producto, sep = " y ") %>% 
    mutate(producto = str_to_lower(producto))

Prod_B_2016x <- Prod_B_2016w %>% 
    filter(grepl('^_', producto))

Prod_B_2016y <- Prod_B_2016x %>%
    filter(!grepl('^_', producto)) %>%
    bind_rows(Prod_B_2016w) %>% 
    filter(producto != "") %>%
    mutate(producto = str_replace_all(producto, "_", " ")) %>% 
    bind_rows(Prod_B_2016v) %>% 
    mutate(val = 1) %>%
    group_by(producto) %>%
    summarise_all("sum") %>%
    mutate(year = "2016") %>%
    filter(producto != "no sabe") %>% 
    filter(producto != "aun no lo siembra") %>% 
    arrange(desc(val)) 
