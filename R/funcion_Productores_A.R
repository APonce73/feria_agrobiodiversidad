funcion_prod_A <- function(database, year1){
    # Para columna productos
    Prod_A_2016_2 <- database %>% 
        select(producto) %>% 
        #    rename(producto = productos) %>% 
        mutate(producto = str_to_lower(producto)) %>% 
        mutate(producto = str_replace(producto, "ma_z", "maiz")) %>%
        mutate(producto = str_replace(producto, "option_8", "amaranto")) %>% 
        mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>% 
        mutate(producto = str_replace(producto, "option_11", "chayote")) %>% 
        mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>% 
        mutate(producto = str_replace(producto, "option_18", "nicuatole")) %>% 
        mutate(producto = str_replace(producto, "x", "plantas_medicinales")) %>% 
        mutate(producto = str_replace(producto, "algod_n", "algodon")) %>%
        mutate(producto = str_replace(producto, "caf", "cafe")) %>%
        mutate(producto = str_replace(producto, "ca_a", "caña")) %>%
        mutate(producto = str_replace(producto, "ceboll_n", "cebollin")) %>%
        mutate(producto = str_replace(producto, "option_67", "cilantro")) %>%
        mutate(producto = str_replace(producto, "n_spero", "nispero")) %>%
        mutate(producto = str_replace(producto, "panela_piloncillo", "piloncillo")) %>%
        mutate(producto = str_replace(producto, "sand_a", "sandia")) %>%
        separate_rows(producto, sep = " ") %>% 
        filter(producto != "otro")
    
    #Para la columna de los otros
    
    Prod_A_2016_3 <- database %>% 
        select(otros) %>% 
        drop_na() %>% 
        separate_rows(otros, sep = ",") %>% 
        mutate(otros = str_to_lower(otros)) %>% 
        mutate(otros = str_replace_all(otros, " ", "_")) %>% 
        mutate(otros = str_replace_all(otros, "\\.", "")) %>% 
        mutate(otros = str_replace_all(otros, "__", "_"))
    
    Prod_A_2016_3a <- Prod_A_2016_3 %>% 
        filter(grepl('^_', otros)) 
    
    str_sub(Prod_A_2016_3a$otros, 1, 1) <- ""
    
    Prod_A_2016_3b <- Prod_A_2016_3 %>% 
        filter(!grepl('^_', otros)) %>% 
        bind_rows(Prod_A_2016_3a) %>% 
        filter(otros != "") %>% 
        rename(producto = otros) %>% 
        bind_rows(Prod_A_2016_2) %>%
        mutate(producto = str_replace(producto, "á", "a")) %>% 
        mutate(producto = str_replace(producto, "é", "e")) %>% 
        mutate(producto = str_replace(producto, "í", "i")) %>% 
        mutate(producto = str_replace(producto, "ó", "o")) %>% 
        mutate(producto = str_replace(producto, "ú", "u")) %>% 
        mutate(producto = recode(producto, "papas" = "papa")) %>% 
        mutate(producto = recode(producto, "aba" = "haba")) %>%
        mutate(producto = recode(producto, "abas" = "haba")) %>%
        mutate(producto = recode(producto, "habas" = "haba")) %>%
        mutate(producto = recode(producto, "chayotes" = "chayote")) %>%
        mutate(producto = recode(producto, "cebolin" = "cebollin")) %>% 
        mutate(producto = recode(producto, "caf" = "cafe"))
    
    
    calabaza <- Prod_A_2016_3b %>% 
        filter(str_detect(producto, "^calabaza")) %>% 
        mutate(producto = "calabaza")
    
    Prod_A_2016_final <- Prod_A_2016_3b %>% 
        filter(!str_detect(producto, "^calabaza")) %>% 
        bind_rows(calabaza) %>% 
        mutate(val = 1) %>% 
        group_by(producto) %>% 
        summarise_all("sum") %>% 
        mutate(year = year1) %>% 
        arrange(desc(val))
    
    rm(calabaza, Prod_A_2016_2, Prod_A_2016_3, Prod_A_2016_3a, Prod_A_2016_3b)
    print(Prod_A_2016_final)
}

#Prod_A_2016_final <-  TablaLL(Prod_A_2016_1)


##  # The original construction
##  
##  # Para columna productos
##  Prod_A_2016_2 <- Prod_A_2016_1 %>% 
##      select(producto) %>% 
##      #    rename(producto = productos) %>% 
##      mutate(producto = str_to_lower(producto)) %>% 
##      mutate(producto = str_replace(producto, "ma_z", "maiz")) %>%
##      mutate(producto = str_replace(producto, "option_8", "amaranto")) %>% 
##      mutate(producto = str_replace(producto, "option_10", "cacahuate")) %>% 
##      mutate(producto = str_replace(producto, "option_11", "chayote")) %>% 
##      mutate(producto = str_replace(producto, "option_13", "hierba_santa")) %>% 
##      mutate(producto = str_replace(producto, "option_18", "nicuatole")) %>% 
##      mutate(producto = str_replace(producto, "x", "plantas_medicinales")) %>% 
##      separate_rows(producto, sep = " ") %>% 
##      filter(producto != "otro")
##  
##  #Para la columna de los otros
##  
##  Prod_A_2016_3 <- Prod_A_2016_1 %>% 
##      select(otros) %>% 
##      drop_na() %>% 
##      separate_rows(otros, sep = ",") %>% 
##      mutate(otros = str_to_lower(otros)) %>% 
##      mutate(otros = str_replace_all(otros, " ", "_")) %>% 
##      mutate(otros = str_replace_all(otros, "\\.", "")) %>% 
##      mutate(otros = str_replace_all(otros, "__", "_"))
##  
##  Prod_A_2016_3a <- Prod_A_2016_3 %>% 
##      filter(grepl('^_', otros)) 
##  
##  str_sub(Prod_A_2016_3a$otros, 1, 1) <- ""
##  
##  Prod_A_2016_3b <- Prod_A_2016_3 %>% 
##      filter(!grepl('^_', otros)) %>% 
##      bind_rows(Prod_A_2016_3a) %>% 
##      filter(otros != "") %>% 
##      rename(producto = otros) %>% 
##      bind_rows(Prod_A_2016_2) %>%
##      mutate(producto = str_replace(producto, "á", "a")) %>% 
##      mutate(producto = str_replace(producto, "é", "e")) %>% 
##      mutate(producto = str_replace(producto, "í", "i")) %>% 
##      mutate(producto = str_replace(producto, "ó", "o")) %>% 
##      mutate(producto = str_replace(producto, "ú", "u")) %>% 
##      mutate(producto = recode(producto, "papas" = "papa")) %>% 
##      mutate(producto = recode(producto, "aba" = "haba")) %>%
##      mutate(producto = recode(producto, "abas" = "haba")) %>%
##      mutate(producto = recode(producto, "habas" = "haba")) %>%
##      mutate(producto = recode(producto, "chayotes" = "chayote")) %>%
##      mutate(producto = recode(producto, "cebolin" = "cebollin"))
##  
##  calabaza <- Prod_A_2016_3b %>% 
##      filter(str_detect(producto, "^calabaza")) %>% 
##      mutate(producto = "calabaza")
##  
##  Prod_A_2016_final <- Prod_A_2016_3b %>% 
##      filter(!str_detect(producto, "^calabaza")) %>% 
##      bind_rows(calabaza) %>% 
##      mutate(val = 1) %>% 
##      group_by(producto) %>% 
##      summarise_all("sum")
##  
##  rm(calabaza, Prod_A_2016_1, Prod_A_2016_2, Prod_A_2016_3, Prod_A_2016_3a, Prod_A_2016_3b)
##  
