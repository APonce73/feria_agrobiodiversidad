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

#source("R/download_data.R")

##################################### 2016  ###################
# Para los intercambios las bases de Prod_B
# ¿Que semillas trajo a intercambiar?
names(Prod_B_2016)
semillas_intercambiar <- function(base_datos, year1){

    Prod_B_2016a <- base_datos %>% 
        select(matches("_Qu_productos_traen_a_exponer")[1]) %>%  
        rename(producto = contains("productos")) %>%  
        mutate(producto = str_to_lower(producto)) %>% 
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
        mutate(producto = str_replace(producto, "x_1", "amaranto")) %>% 
        mutate(producto = str_replace(producto, "option_10", "plantas_medicinales")) %>% 
        mutate(producto = str_replace(producto, "x", "chayote")) %>% 
        mutate(producto = str_replace(producto, "option_11", "tortillas")) %>% 
        # mutate(producto = str_replace(producto, "option_18", "nicuatole")) %>% 
        # mutate(producto = str_replace(producto, "x", "plantas_medicinales")) %>% 
        # mutate(producto = str_replace(producto, "algod_n", "algodón")) %>%
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
    
    
    Prod_B_2016b <- base_datos %>% 
        select(contains("otros_productos")) %>% 
        rename(producto = contains("otros_productos")) %>%
        drop_na() %>% 
        separate_rows(producto, sep = ", ") %>% 
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
        mutate(producto = recode(producto, "ajongolin" = "ajonjolí")) %>%
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
        mutate(year = year1) %>% 
        arrange(desc(val)) %>% 
        filter(val > 1)

    rm(Prod_B_2016a, Prod_B_2016b, Prod_B_2016c)
    print(Prod_B_2016d)
        
}


##############
# Adquiriste semillas en ferias pasadas?
adquiriste_semillas <- function(base_datos, year1){
    Prod_B_2016e <- base_datos %>% 
        select(contains("Adquiriste_semillas_en_ferias")[1]) %>% 
        rename(producto = contains("semillas_en_ferias")) %>%  
        mutate(producto = str_to_lower(producto)) %>% 
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
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
    
    Prod_B_2016f <- base_datos %>% 
        select(contains("Si_seleccion_otro_e_i_en_ferias_pasadas")) %>% 
        rename(producto = contains("Si_seleccion_otro_e_i_en_ferias_pasadas")) %>%  
        drop_na() %>% 
        separate_rows(producto, sep = ", ") %>% 
        separate_rows(producto, sep = ",") %>% 
        separate_rows(producto, sep = " y ") %>% 
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016g <- Prod_B_2016f %>% 
        filter(grepl('^_', producto))
    
    Prod_B_2016h <- Prod_B_2016g %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016f) %>%
        filter(producto != "") %>%
        bind_rows(Prod_B_2016e) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>%
        mutate(val = 1) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        arrange(desc(val))
    #    filter(val > 1)
    rm(Prod_B_2016e, Prod_B_2016f, Prod_B_2016g)
    print(Prod_B_2016h)
}

#############
#Las semillas que adquiriste en ferias pasadas las sembrastes?
sembraste_semillas <- function(base_datos, year1){
    Prod_B_2016i <- base_datos %>% 
        select(contains("semillas_que_adquiriste_e")[1]) %>% 
        rename(producto = contains("semillas_que_adquiriste")) %>%  
        mutate(producto = str_to_lower(producto)) %>% 
        mutate(producto = str_replace(producto, "option_1", "maíz")) %>% 
        mutate(producto = str_replace(producto, "option_2", "frijol")) %>%
        mutate(producto = str_replace(producto, "option_9", "amaranto")) %>%
        mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>% 
        #mutate(producto = str_replace(producto, "", "calabaza")) %>%
        mutate(producto = str_replace(producto, "option_12", "hierba santa")) %>% 
        mutate(producto = str_replace(producto, "option_13", "limones")) %>%
        mutate(producto = str_replace(producto, "option_14", "miltomate")) %>%
        mutate(producto = str_replace(producto, "option_15", "naranjas")) %>%
        mutate(producto = str_replace(producto, "option_16", "nopales")) %>%
       # mutate(producto = str_replace(producto, "maíz3", "maíz")) %>% 
        mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
        separate_rows(producto, sep = " ") %>% 
        filter(producto != "otro") %>% 
        filter(producto != "ninguna")
    
    Prod_B_2016j <- base_datos %>% 
        select(contains("Si_seleccion_otro_e_s_semillas_que_sembr")) %>% 
        rename(producto = contains("Si_seleccion_otro_e_s_semillas_que_sembr")) %>%  
        drop_na() %>% 
        separate_rows(producto, sep = ", ") %>% 
        separate_rows(producto, sep = ",") %>% 
        separate_rows(producto, sep = " y ") %>% 
       # mutate(producto = str_replace(producto, "calabasita", "calabaza")) %>%
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016k <- Prod_B_2016j %>% 
        filter(grepl('^_', producto))
    
    Prod_B_2016L <- Prod_B_2016k %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016j) %>% 
        filter(producto != "") %>%
        bind_rows(Prod_B_2016i) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>% 
        mutate(val = 1) %>%
        mutate(producto = str_replace(producto, "maíz3", "maíz")) %>%
        mutate(producto = str_replace(producto, "semilla de durazno", "semillas de durazno")) %>%
        mutate(producto = str_replace(producto, "calabacita", "calazaba")) %>%
        mutate(producto = str_replace(producto, "calabasita", "calazaba")) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>% 
        arrange(desc(val)) 
    rm(Prod_B_2016i, Prod_B_2016j,Prod_B_2016k)
    print(Prod_B_2016L)
        
}

#Tabla3 <-  sembraste_semillas(base_datos = Prod_B_2016, year1 = "2016")
############

#Las semillas que adquiriste y sembraste en ferias pasadas ¿se te dieron?

crecieron_semillas <- function(base_datos, year1){
    Prod_B_2016m <- base_datos %>% 
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
        mutate(producto = str_replace(producto, "option_1", "maíz")) %>% 
        mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
        separate_rows(producto, sep = " ") %>% 
        filter(producto != "otro") %>% 
        filter(producto != "ninguna")
    
    Prod_B_2016n <- base_datos %>% 
        select(contains("Si_seleccion_otro_e_las_que_se_le_dieron")) %>% 
        rename(producto = contains("Si_seleccion_otro_e_las_que_se_le_dieron")) %>%  
        drop_na() %>% 
        separate_rows(producto, sep = ", ") %>% 
        separate_rows(producto, sep = ",") %>% 
        separate_rows(producto, sep = " y ") %>% 
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016p <- Prod_B_2016n %>% 
        filter(grepl('^_', producto))
    
    Prod_B_2016q <- Prod_B_2016p %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016n) %>% 
        filter(producto != "") %>%
        bind_rows(Prod_B_2016m) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>% 
        mutate(val = 1) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>% 
        arrange(desc(val)) 
    rm(Prod_B_2016m, Prod_B_2016n, Prod_B_2016p)
    print(Prod_B_2016q)
}

#Tabla4 <- crecieron_semillas(base_datos = Prod_B_2016, year1 = "2016")
    
###########
#¿Continua sembrando esas semillas?

continua_sembrando <- function(base_datos, year1) {
    Prod_B_2016r <- base_datos %>%
        select(contains("_Continua_sembrando_esas_semil")[1]) %>%
        rename(producto = contains("_Continua_sembrando_esas_semil")) %>%
        mutate(producto = str_to_lower(producto)) %>%
        mutate(producto = str_replace(producto, "x", "amaranto")) %>%
        mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
        mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
        mutate(producto = str_replace(producto, "option_12", "hierba_santa")) %>%
        mutate(producto = str_replace(producto, "option_13", "limones")) %>%
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
        mutate(producto = str_replace(producto, "option_14", "miltomate")) %>%
        #mutate(producto = str_replace(producto, "option_15", "naranjas")) %>%
        mutate(producto = str_replace(producto, "option_16", "nopales")) %>%
        mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>%
        separate_rows(producto, sep = " ") %>%
        filter(producto != "otro") %>%
        filter(producto != "ninguna")
    
    Prod_B_2016s <- base_datos %>%
        select(contains("seleccion_otro_e_e_contin")) %>%
        rename(producto = contains("seleccion_otro_e_e_contin")) %>%
        drop_na() %>%
        separate_rows(producto, sep = ", ") %>%
        separate_rows(producto, sep = ",") %>%
        separate_rows(producto, sep = " y ") %>%
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016t <- Prod_B_2016s %>%
        filter(grepl('^_', producto))
    
    Prod_B_2016u <- Prod_B_2016t %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016s) %>%
        filter(producto != "") %>%
        bind_rows(Prod_B_2016r) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>%
        mutate(val = 1) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>%
        filter(producto != "aun no lo siembra") %>%
        arrange(desc(val))
    
    rm(Prod_B_2016r, Prod_B_2016s, Prod_B_2016t)
    
    print(Prod_B_2016u)
}

#Tabla4.1 <- continua_sembrando(base_datos = Prod_B_2016, year1 = "2016")

#########
# En esta feria ¿Qué semillas has adquirido?
semillas_adquiridas_ahora <- function(base_datos, year1){
    Prod_B_2016v <- base_datos %>% 
        select(contains("En_esta_feria_Qu_semillas_ha")[1]) %>%  
        rename(producto = contains("En_esta_feria_Qu_semillas_ha")) %>% 
        mutate(producto = str_to_lower(producto)) %>% 
        #mutate(producto = str_replace(producto, "x", "amaranto")) %>%
        #mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
        mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
        mutate(producto = str_replace(producto, "option_12", "hierba santa")) %>%
        mutate(producto = str_replace(producto, "option_13", "limones")) %>% 
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
        mutate(producto = str_replace(producto, "option_14", "miltomate")) %>% 
        mutate(producto = str_replace(producto, "option_15", "naranjas")) %>%
        mutate(producto = str_replace(producto, "option_16", "nopales")) %>%
        mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>% 
        separate_rows(producto, sep = " ") %>% 
        filter(producto != "otro") %>% 
        filter(producto != "ninguna")
    
    Prod_B_2016w <- base_datos %>% 
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
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>% 
        filter(producto != "aun no lo siembra") %>% 
        arrange(desc(val)) 
    rm(Prod_B_2016v, Prod_B_2016w, Prod_B_2016x)
    print(Prod_B_2016y)
}

#Tabla5 <- semillas_adquiridas_ahora(base_datos =  Prod_B_2016, year1 = "2016")

#Cambios en la feria realizada

##################################### 2017, 2018 y 2019  ###################

# Para los intercambios las bases de Prod_B
# ¿Que semillas trajo a intercambiar?
names(Prod_B_2017)

#semillas_intercambiar <- function(base_datos, year1){
#    
#    Prod_B_2016a <- base_datos %>% 
#        select(matches("_Qu_productos_traen_a_exponer")[1]) %>%  
#        rename(producto = contains("productos")) %>%  
#        mutate(producto = str_to_lower(producto)) %>% 
#        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
#        mutate(producto = str_replace(producto, "ajonjol", "ajonjolí")) %>%
#        mutate(producto = str_replace(producto, "algod_n", "algodón")) %>%
#        mutate(producto = str_replace(producto, "x_1", "amaranto")) %>% 
#        mutate(producto = str_replace(producto, "option_10", "plantas_medicinales")) %>% 
#        mutate(producto = str_replace(producto, "x", "chayote")) %>% 
#        mutate(producto = str_replace(producto, "option_11", "tortillas")) %>% 
#        # mutate(producto = str_replace(producto, "option_18", "nicuatole")) %>% 
#        # mutate(producto = str_replace(producto, "x", "plantas_medicinales")) %>% 
#        # mutate(producto = str_replace(producto, "caf", "cafe")) %>%
#        # mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
#        # mutate(producto = str_replace(producto, "ceboll_n", "cebollin")) %>%
#        # mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
#        # mutate(producto = str_replace(producto, "n_spero", "nispero")) %>%
#        # mutate(producto = str_replace(producto, "panela_piloncillo", "piloncillo")) %>%
#        # mutate(producto = str_replace(producto, "sand_a", "sandia")) %>%
#        separate_rows(producto, sep = " ") %>% 
#        filter(producto != "otro") %>% 
#        mutate(producto = str_to_lower(producto))
#    
#    
#    Prod_B_2016b <- base_datos %>% 
#        select(contains("otros_productos")) %>% 
#        rename(producto = contains("otros_productos")) %>%
#        drop_na() %>% 
#        separate_rows(producto, sep = ", ") %>% 
#        separate_rows(producto, sep = ",") %>% 
#        separate_rows(producto, sep = " y ") %>% 
#        mutate(producto = str_replace(producto, ":", "")) %>% 
#        mutate(producto = str_to_lower(producto)) %>% 
#        mutate(producto = str_replace_all(producto, " ", "_")) %>% 
#        mutate(producto = str_replace_all(producto, "\\.", "")) %>% 
#        mutate(producto = str_replace_all(producto, "__", "_"))
#    
#    Prod_B_2016c <- Prod_B_2016b %>% 
#        filter(grepl('^_', producto)) 
#    
#    str_sub(Prod_B_2016c$producto, 1, 1) <- ""
#    
#    Prod_B_2016d <- Prod_B_2016b %>% 
#        filter(!grepl('^_', producto)) %>% 
#        bind_rows(Prod_B_2016c) %>% 
#        filter(producto != "") %>% 
#        mutate(producto = str_replace_all(producto, "_", " ")) %>% 
#        bind_rows(Prod_B_2016a) %>%
#        mutate(producto = str_replace(producto, "á", "a")) %>% 
#        mutate(producto = str_replace(producto, "é", "e")) %>% 
#        mutate(producto = str_replace(producto, "í", "i")) %>% 
#        mutate(producto = str_replace(producto, "ó", "o")) %>% 
#        mutate(producto = str_replace(producto, "ú", "u")) %>% 
#        mutate(producto = recode(producto, "cola de carballo" = "cola de caballo")) %>% 
#        mutate(producto = recode(producto, "coyol coroso" = "coroso")) %>%
#        mutate(producto = recode(producto, "coroso" = "corozo")) %>%
#        mutate(producto = recode(producto, "habas" = "haba")) %>%
#        mutate(producto = recode(producto, "ajongolin" = "ajonjolí")) %>%
#        mutate(producto = recode(producto, "ags" = "NA")) %>% 
#        mutate(producto = recode(producto, "mispero" = "nispero")) %>% 
#        mutate(producto = recode(producto, "peino" = "pepino")) %>% 
#        mutate(producto = recode(producto, "aguacate bola criollo" = "aguacate criollo")) %>% 
#        mutate(producto = recode(producto, "carambolo" = "carambola")) %>% 
#        mutate(producto = recode(producto, "mezcales" = "mezcal")) %>% 
#        mutate(producto = recode(producto, "abas" = "haba")) %>% 
#        mutate(producto = recode(producto, "limas" = "lima")) %>% 
#        mutate(producto = recode(producto, "chirimollas" = "chirimoya")) %>% 
#        #mutate(producto = recode(producto, "caf" = "cafe")) %>% 
#        #mutate(producto = recode(producto, "caf" = "cafe")) %>% 
#        mutate(val = 1) %>% 
#        group_by(producto) %>% 
#        summarise_all("sum") %>% 
#        mutate(year = year1) %>% 
#        arrange(desc(val)) %>% 
#        filter(val > 1)
#    
#    rm(Prod_B_2016a, Prod_B_2016b, Prod_B_2016c)
#    print(Prod_B_2016d)
#    
#}
#
#Tabla1 <- semillas_intercambiar(base_datos = Prod_B_2016, year1 = 2016)

##############
# Adquiriste semillas en ferias pasadas?
adquiriste_semillas_1 <- function(base_datos, year1){
    Prod_B_2016e <- base_datos %>%
        select(contains("_Cu_ntos_tipos_o_variedades_de")[1]) %>%
        rename(producto = contains("tipos_o_variedades")) %>%
        mutate(producto = str_to_lower(producto)) %>%
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
        mutate(producto = str_replace(producto, "ajonjol", "ajonjolí")) %>%
        mutate(producto = str_replace(producto, "algod_n", "algodón")) %>%
        mutate(producto = str_replace(producto, "option_8", "amaranto")) %>%
        mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
        mutate(producto = str_replace(producto, "caf", "cafe")) %>%
        mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
        mutate(producto = str_replace(producto, "ceboll_n", "cebollín")) %>%
        mutate(producto = str_replace(producto, "cebollin", "cebollín")) %>%
        mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
        mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
        mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>%
        mutate(producto = str_replace(producto, "n_spero", "níspero")) %>%
        mutate(producto = str_replace(producto, "x", "plantas medicinales")) %>%
        mutate(producto = str_replace(producto, "pl_tano", "plátano")) %>%
        mutate(producto = str_replace(producto, "sand_a", "sandía")) %>%
        # mutate(producto = str_replace(producto, "option_14", "limones")) %>%
        # mutate(producto = str_replace(producto, "option_15", "miltomate")) %>%
        # mutate(producto = str_replace(producto, "option_16", "naranjas")) %>%
        # mutate(producto = str_replace(producto, "x_1", "plantas medicinales")) %>%
        separate_rows(producto, sep = " ") %>%
        filter(producto != "otro") %>%
        filter(producto != "ninguna")
    
    Prod_B_2016f <- base_datos %>% 
        select(contains("Si_seleccion_otro_e_i_en_ferias_pasadas")[1]) %>% 
        rename(producto = contains("Si_seleccion_otro")) %>% 
        drop_na() %>% 
        separate_rows(producto, sep = ", ") %>% 
        separate_rows(producto, sep = ",") %>% 
        separate_rows(producto, sep = " y ") %>% 
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016g <- Prod_B_2016f %>% 
        filter(grepl('^_', producto))
    
    Prod_B_2016h <- Prod_B_2016g %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016f) %>%
        filter(producto != "") %>%
        bind_rows(Prod_B_2016e) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>%
        mutate(val = 1) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        arrange(desc(val))
    #    filter(val > 1)
    rm(Prod_B_2016e, Prod_B_2016f, Prod_B_2016g)
    print(Prod_B_2016h)
}

#Tabla2a <- adquiriste_semillas_1(base_datos = Prod_B_2017, year1 = "2017")

#############
#Las semillas que adquiriste en ferias pasadas las sembrastes?
sembraste_semillas_1 <- function(base_datos, year1){
    Prod_B_2016i <- base_datos %>% 
        select(contains("_Cu_ntos_tipos_o_variedades_de_001")[1]) %>% 
        rename(producto = contains("_Cu_ntos_tipos_o_variedades_de_001")) %>%    
        mutate(producto = str_to_lower(producto)) %>% 
            mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
            mutate(producto = str_replace(producto, "ajonjol", "ajonjolí")) %>%
            mutate(producto = str_replace(producto, "algod_n", "algodón")) %>%
            mutate(producto = str_replace(producto, "option_8", "amaranto")) %>%
            mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
            mutate(producto = str_replace(producto, "caf", "cafe")) %>%
            mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
            mutate(producto = str_replace(producto, "ceboll_n", "cebollín")) %>%
            mutate(producto = str_replace(producto, "cebollin", "cebollín")) %>%
            mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
            mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
            mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>%
            mutate(producto = str_replace(producto, "n_spero", "níspero")) %>%
            mutate(producto = str_replace(producto, "x", "plantas medicinales")) %>%
            mutate(producto = str_replace(producto, "pl_tano", "plátano")) %>%
            mutate(producto = str_replace(producto, "sand_a", "sandía")) %>%
            mutate(producto = str_replace(producto, "guan_bana", "guanábana")) %>%
        separate_rows(producto, sep = " ") %>% 
        filter(producto != "otro") %>% 
        filter(producto != "ninguna")
    
    Prod_B_2016j <- base_datos %>% 
        select(contains("Si_seleccion_otro_e_s_semillas_que_sembr")) %>% 
        rename(producto = contains("Si_seleccion_otro_e_s_semillas_que_sembr")) %>% 
        drop_na() %>% 
        separate_rows(producto, sep = ", ") %>% 
        separate_rows(producto, sep = ",") %>% 
        separate_rows(producto, sep = " y ") %>% 
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016k <- Prod_B_2016j %>% 
        filter(grepl('^_', producto))
    
    Prod_B_2016L <- Prod_B_2016k %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016j) %>% 
        filter(producto != "") %>%
        bind_rows(Prod_B_2016i) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>% 
        mutate(val = 1) %>%
        mutate(producto = str_replace(producto, "maiz3", "maíz")) %>%
        mutate(producto = str_replace(producto, "semilla de durazno", "semillas de durazno")) %>%
        mutate(producto = str_replace(producto, "calabacita", "calazaba")) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>% 
        arrange(desc(val)) 
    rm(Prod_B_2016i, Prod_B_2016j,Prod_B_2016k)
    print(Prod_B_2016L)
    
}

#Tabla3a <-  sembraste_semillas_1(base_datos = Prod_B_2017, year1 = "2017")

############
#Las semillas que adquiriste y sembraste en ferias pasadas ¿se te dieron?

crecieron_semillas_1 <- function(base_datos, year1){
    Prod_B_2016m <- base_datos %>% 
        select(contains("_Cu_ntos_tipos_o_variedades_de_002")[1]) %>% 
        rename(producto = contains("_Cu_ntos_tipos_o_variedades_de_002")) %>%  
        mutate(producto = str_to_lower(producto)) %>% 
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
        mutate(producto = str_replace(producto, "ajonjol", "ajonjolí")) %>%
        mutate(producto = str_replace(producto, "algod_n", "algodón")) %>%
        mutate(producto = str_replace(producto, "option_8", "amaranto")) %>%
        mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
        mutate(producto = str_replace(producto, "caf", "cafe")) %>%
        mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
        mutate(producto = str_replace(producto, "ceboll_n", "cebollín")) %>%
        mutate(producto = str_replace(producto, "cebollin", "cebollín")) %>%
        mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
        mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
        mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>%
        mutate(producto = str_replace(producto, "n_spero", "níspero")) %>%
        mutate(producto = str_replace(producto, "x", "plantas medicinales")) %>%
        mutate(producto = str_replace(producto, "pl_tano", "plátano")) %>%
        mutate(producto = str_replace(producto, "sand_a", "sandía")) %>%
        mutate(producto = str_replace(producto, "guan_bana", "guanábana")) %>% 
        separate_rows(producto, sep = " ") %>% 
        filter(producto != "otro") %>% 
        filter(producto != "ninguna")
    
    Prod_B_2016n <- base_datos %>% 
        select(contains("Si_seleccion_otro_e_las_que_se_le_dieron")) %>% 
        rename(producto = contains("Si_seleccion_otro_e_las_que_se_le_dieron")) %>%  
        drop_na() %>% 
        separate_rows(producto, sep = ", ") %>% 
        separate_rows(producto, sep = ",") %>% 
        separate_rows(producto, sep = " y ") %>% 
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016p <- Prod_B_2016n %>% 
        filter(grepl('^_', producto))
    
    Prod_B_2016q <- Prod_B_2016p %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016n) %>% 
        filter(producto != "") %>%
        bind_rows(Prod_B_2016m) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>% 
        mutate(val = 1) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>% 
        arrange(desc(val)) 
    rm(Prod_B_2016m, Prod_B_2016n, Prod_B_2016p)
    print(Prod_B_2016q)
}

#Tabla4a <- crecieron_semillas_1(base_datos = Prod_B_2017, year1 = "2017")

###########
#¿Continua sembrando esas semillas?

continua_sembrando_1 <- function(base_datos, year1) {
    Prod_B_2016r <- base_datos %>%
        select(contains("_Cu_ntos_tipos_o_variedades_de_003")[1]) %>%
        rename(producto = contains("_Cu_ntos_tipos_o_variedades_de_003")) %>%
        mutate(producto = str_to_lower(producto)) %>%
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
        mutate(producto = str_replace(producto, "ajonjol", "ajonjolí")) %>%
        mutate(producto = str_replace(producto, "algod_n", "algodón")) %>%
        mutate(producto = str_replace(producto, "option_8", "amaranto")) %>%
        mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
        mutate(producto = str_replace(producto, "caf", "cafe")) %>%
        mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
        mutate(producto = str_replace(producto, "ceboll_n", "cebollín")) %>%
        mutate(producto = str_replace(producto, "cebollin", "cebollín")) %>%
        mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
        mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
        mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>%
        mutate(producto = str_replace(producto, "n_spero", "níspero")) %>%
        mutate(producto = str_replace(producto, "x", "plantas medicinales")) %>%
        mutate(producto = str_replace(producto, "pl_tano", "plátano")) %>%
        mutate(producto = str_replace(producto, "sand_a", "sandía")) %>%
        mutate(producto = str_replace(producto, "guan_bana", "guanábana")) %>% 
        separate_rows(producto, sep = " ") %>%
        filter(producto != "otro") %>%
        filter(producto != "ninguna")
    
    Prod_B_2016s <- base_datos %>%
        select(contains("Si_seleccion_otro_e_e_contin_a_sembrando")) %>%
        rename(producto = contains("Si_seleccion_otro_e_e_contin_a_sembrando")) %>%
        drop_na() %>%
        separate_rows(producto, sep = ", ") %>%
        separate_rows(producto, sep = ",") %>%
        separate_rows(producto, sep = " y ") %>%
        mutate(producto = str_to_lower(producto))
    
    Prod_B_2016t <- Prod_B_2016s %>%
        filter(grepl('^_', producto))
    
    Prod_B_2016u <- Prod_B_2016t %>%
        filter(!grepl('^_', producto)) %>%
        bind_rows(Prod_B_2016s) %>%
        filter(producto != "") %>%
        bind_rows(Prod_B_2016r) %>%
        mutate(producto = str_replace_all(producto, "_", " ")) %>%
        mutate(val = 1) %>%
        group_by(producto) %>%
        summarise_all("sum") %>%
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>%
        filter(producto != "aun no lo siembra") %>%
        arrange(desc(val))
    
    rm(Prod_B_2016r, Prod_B_2016s, Prod_B_2016t)
    
    print(Prod_B_2016u)
}

#Tabla4.1a <- continua_sembrando_1(base_datos = Prod_B_2017, year1 = "2017")

#########
# En esta feria ¿Qué semillas has adquirido?
semillas_adquiridas_ahora_1 <- function(base_datos, year1){
    Prod_B_2016v <- base_datos %>% 
        select(contains("_Cu_ntos_tipos_o_variedades_de_004")[1]) %>%
        rename(producto = contains("_Cu_ntos_tipos_o_variedades_de_004")) %>%
        mutate(producto = str_to_lower(producto)) %>%
        mutate(producto = str_replace(producto, "ma_z", "maíz")) %>%
        mutate(producto = str_replace(producto, "ajonjol", "ajonjolí")) %>%
        mutate(producto = str_replace(producto, "algod_n", "algodón")) %>%
        mutate(producto = str_replace(producto, "option_8", "amaranto")) %>%
        mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>%
        mutate(producto = str_replace(producto, "caf", "cafe")) %>%
        mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
        mutate(producto = str_replace(producto, "ceboll_n", "cebollín")) %>%
        mutate(producto = str_replace(producto, "cebollin", "cebollín")) %>%
        mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
        mutate(producto = str_replace(producto, "option_11", "chayote")) %>%
        mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>%
        mutate(producto = str_replace(producto, "n_spero", "níspero")) %>%
        mutate(producto = str_replace(producto, "x", "plantas medicinales")) %>%
        mutate(producto = str_replace(producto, "pl_tano", "plátano")) %>%
        mutate(producto = str_replace(producto, "sand_a", "sandía")) %>%
        mutate(producto = str_replace(producto, "guan_bana", "guanábana")) %>% 
        separate_rows(producto, sep = " ") %>% 
        filter(producto != "otro") %>% 
        filter(producto != "ninguna")
    
    Prod_B_2016w <- base_datos %>% 
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
        mutate(year = year1) %>%
        filter(producto != "no sabe") %>% 
        filter(producto != "aun no lo siembra") %>% 
        arrange(desc(val)) 
    rm(Prod_B_2016v, Prod_B_2016w, Prod_B_2016x)
    print(Prod_B_2016y)
}

#Tabla5a <- semillas_adquiridas_ahora_1(base_datos =  Prod_B_2017, year1 = "2017")

#Cambios en la feria realizada
