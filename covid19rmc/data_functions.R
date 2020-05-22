###
get_my_stats = function(input, k = 100000){
  ## requer agrupamento por cidade/estado/etc
  input %>%
    mutate(CFR         = round(deaths/confirmed*100, 2),
           cases100k   = round(confirmed/estimated_population_2019*k, 2),
           deaths100k  = round(deaths/estimated_population_2019*k, 2),
           daily_cases = confirmed - lag(confirmed, n=1, default=0))
}

get_data_brasil = function(input){
  input %>% filter(place_type == "state") %>% 
    select(-confirmed_per_100k_inhabitants, -death_rate) %>%
    group_by(state) %>% 
    arrange(date) %>% 
    get_my_stats() %>% 
    ungroup()
}

get_data = function(input, this_state="SP", RMC, COI, SOI){
  dados_sp = input %>% filter(state == this_state)
  
  dados_cid_RMC = dados_sp %>% semi_join(RMC, by='city_ibge_code') %>% 
    mutate(place_type = "city_rmc")
  
  dados_reg_RMC = dados_cid_RMC %>% group_by(date) %>%
    summarise(state="SP", city="RMC", place_type="region_rmc", confirmed=sum(confirmed),
              deaths=sum(deaths), is_last=FALSE, estimated_population_2019=3224443,
              city_ibge_code=NA, confirmed_per_100k_inhabitants=NA, death_rate=NA) %>% 
    ungroup() %>% arrange(date)
  dados_reg_RMC$is_last[nrow(dados_reg_RMC)] = TRUE
  
  dados_cid_COI = dados_sp %>% semi_join(COI, by='city_ibge_code') %>% 
    mutate(place_type = "city_coi")
  
  dados_reg_RMCCOI = dados_reg_RMC %>% bind_rows(dados_cid_COI) %>% group_by(date) %>%
    summarise(state="SP", city="RMC + Limeira + Piracicaba",
              place_type="region_rmccoi", confirmed=sum(confirmed),
              deaths=sum(deaths), is_last=FALSE, estimated_population_2019=3224443+306114+404142,
              city_ibge_code=NA, confirmed_per_100k_inhabitants=NA, death_rate=NA) %>% 
    ungroup() %>% arrange(date)
  dados_reg_RMCCOI$is_last[nrow(dados_reg_RMCCOI)] = TRUE
  
  dados_est_SP  = dados_sp %>% filter(place_type == "state") %>% 
    mutate(city="SP (Estado)")
  
  dados_cid_RMC %>% bind_rows(dados_cid_COI) %>% bind_rows(dados_reg_RMC) %>%
    bind_rows(dados_reg_RMCCOI) %>% bind_rows(dados_est_SP) %>% 
    select(-confirmed_per_100k_inhabitants, -death_rate) %>%
    group_by(city) %>% 
    arrange(date) %>% 
    get_my_stats() %>% 
    ungroup()
}

get_cities = function(data){
  vec = data %>% arrange(city) %>% select(city) %>% distinct() %>% pull()
  names(vec) = vec
  as.list(vec)
}
 
get_base_municipios = function(this_muni="SP", this_year=2018){
  fname = paste0("base_muni_", this_muni, "_", this_year, ".rds")
  fname = file.path("gen_data", fname)
  if (!file.exists(fname)){
    base_muni = read_municipality(code_muni=this_muni, year=this_year, showProgress = FALSE)
    saveRDS(base_muni, file=fname)
    return(base_muni)
  }
  readRDS(fname)
}

get_base_estados = function(this_state="all", this_year=2018){
  fname = paste0("base_estado_", this_state, "_", this_year, ".rds")
  fname = file.path("gen_data", fname)
  if (!file.exists(fname)){
    base_state = read_state(code_state=this_state, year=this_year, showProgress = FALSE)
    saveRDS(base_state, file=fname)
    return(base_state)
  }
  readRDS(fname)
}


## R0
get_R = function(input){
  ref = tibble(date = seq(min(input$date),
                          max(input$date), by=1))
  
  input = ref %>% left_join(input, by='date') %>%
    mutate(cases = ifelse(is.na(cases), 0L, as.integer(cases))) %>% 
    rename(dates=date, I=cases)
  
  library(EpiEstim)
  
  myconfig = make_config(min_mean_si=3.7, mean_si=4.7,
                         max_mean_si=6, min_std_si=1.9, std_si=2.9,
                         std_mean_si=2.9, max_std_si=4.9, mean_prior=2.6,
                         std_prior=2, si_parametric_distr='lognormal')
  estimate_R(input, method="parametric_si", config = myconfig)
}

####
#### DRS

get_drs_sp_data = function(){
  library(readxl)
  fname = "drs_sp.rds"
  fname = file.path("gen_data", fname)
  if (!file.exists(fname)){
    municipios_sp_ibge = read_excel("data/RELATORIO_DTB_BRASIL_MUNICIPIO.xls") %>% 
      select(Nome_UF, Nome_Município, `Código Município Completo`) %>% 
      rename(municipio=Nome_Município, cod_mun = `Código Município Completo`) %>% 
      filter(Nome_UF == "São Paulo") %>% 
      mutate(municipio=toupper(municipio))
    drs_sp = read_excel("data/drs_sp.xlsx") %>%
      left_join(municipios_sp_ibge, by=c("city"="municipio")) %>% 
      select(-Nome_UF)
    pop_data = read_excel("data/estimativa_TCU_2019_20200427.xls",
                          sheet = "Municípios", range = "A2:E5572") %>% 
      rename(uf = UF, cod_uf = `COD. UF`, cod_mun = `COD. MUNIC`,
             muni = `NOME DO MUNICÍPIO`, pop = `POPULAÇÃO ESTIMADA`) %>% 
      mutate(cod_mun = paste0(cod_uf, cod_mun),
             pop = str_remove_all(pop, "\\."),
             pop = str_remove_all(pop, "\\([0-9]+\\)"),
             pop = as.integer(pop)) %>%
      select(cod_mun, pop)
    drs_sp = drs_sp %>%
      left_join(pop_data, by="cod_mun") %>% 
      group_by(region) %>% 
      mutate(pop_region = sum(pop)) %>% 
      ungroup()
    saveRDS(drs_sp, fname)
    return(drs_sp)
    ## o que tem no brasilio() que não tem no IBGE/DRS
    ## mun_sp é um merge após as DRS, a provavel causa
    ## é as cidades não estarem na DRS
    # tmp %>% anti_join(municipios_sp, by="city") %>% 
    # select(city) %>% distinct() %>% arrange(city)
    
    ## o que tem no IBGE que não tem no brasilio() 
    ## provavelmente pq não tem casos
    # municipios_sp %>% anti_join(tmp, by="city") %>% 
    #   select(city) %>% distinct() %>% arrange(city)
    
  }
  readRDS(fname)
}

get_drs_covid_data = function(input){
  input %>% 
    mutate(city=toupper(city)) %>% 
    filter(state == "SP", place_type == "city") %>% 
    select(date, city, confirmed, deaths, estimated_population_2019, is_last) %>% 
    right_join(get_drs_sp_data(), by="city") %>% 
    filter(!is.na(date)) %>% 
    group_by(region, date) %>% 
    summarise(confirmed = sum(confirmed, na.rm=TRUE),
              deaths = sum(deaths, na.rm=TRUE),
              city = name[1],
              estimated_population_2019 = pop_region[1]) %>% 
    ungroup() %>% 
    mutate(is_last = FALSE) %>% 
    group_by(region, city) %>% 
    arrange(date) %>% 
    mutate(is_last = c(is_last[-n()], TRUE)) %>% 
    get_my_stats() %>% 
    ungroup()
}

get_drs_shp = function(){
  fname = "mapa_drs_sp.rds"
  fname = file.path("gen_data", fname)
  if (!file.exists(fname)){
    library(sf)
    mapas_sp = get_base_municipios("SP", 2018) %>%
      left_join(get_drs_sp_data() %>%
                  mutate(cod_mun=as.integer(cod_mun)),
                by=c("code_muni" = "cod_mun")) %>%
      select(region, name) %>% 
      transmute(city = paste(region, name, sep=" - ")) %>% 
      group_by(city)
    
    regs = unique(mapas_sp$city)
    lst = vector('list', length(regs))
    names(lst) = regs
    for (reg in regs){
      lst[[reg]] = st_combine(mapas_sp %>% filter(city == reg)) %>%
        st_buffer(0.001) %>% 
        merge(data.frame(city=reg, stringsAsFactors = FALSE))
    }
    lst = do.call(rbind, lst) %>% st_as_sf()
    saveRDS(lst, fname)
    return(lst)
  }
  readRDS(fname)
}

