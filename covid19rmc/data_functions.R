###
get_data = function(this_state="SP", RMC, COI, SOI){
  dados_sp = brasilio() %>% filter(state == this_state)
  
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
    mutate(CFR         = round(deaths/confirmed*100, 2),
           cases100k   = round(confirmed/estimated_population_2019*100000, 2),
           deaths100k  = round(deaths/estimated_population_2019*100000, 2),
           daily_cases = confirmed - lag(confirmed, n=1, default=0)) %>% 
    ungroup()
}

get_cities = function(data){
  vec = data %>% arrange(city) %>% select(city) %>% distinct() %>% pull()
  names(vec) = vec
  as.list(vec)
}
 
get_base_municipios = function(this_muni="SP", this_year=2018){
  fname = "base_muni.rds"
  if (!file.exists(fname)){
    base_muni = read_municipality(code_muni=this_muni, year=this_year, showProgress = FALSE)
    saveRDS(base_muni, file=fname)
    return(base_muni)
  }
  readRDS(fname)
}