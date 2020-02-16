
create.lv.velden <- function(){
  
  
  lv.sub <- lv_connections %>% 
    # remove dubbele veld.id per xy
    group_by(x, y) %>% filter(row_number() == 1) 
  
  xy.afronding <- 75
  lv.trian <- lv.sub %>% 
    # to make group for filter
    mutate(xx= round(x/xy.afronding), yy = round(y/xy.afronding)) %>%
    group_by(xx, yy, pc6) %>% 
    
    filter(n_distinct(veld.id) > 1 | x == max(x) | x == min(x) |
             y == max(y) | y == min(y)) %>%
    ungroup() %>%
    mutate(row = row_number()) %>% ungroup() 
  
  veld.id.voronoi <- delaunayn(
    lv.trian %>% select(x, y) %>% distinct() %>% mutate_all(as.numeric) %>% as.matrix(),
    options = "Pp") %>% as.data.frame() %>% mutate(r = row_number()) %>%
    gather("col", "row", -r) %>%
    left_join(lv.trian %>% select(veld.id, row), by = "row") %>%
    group_by(r) %>% arrange(r) %>% mutate(n = n_distinct(veld.id)) %>%
    #filter alle punten met een connectie naar een veld met een ander id
    filter(n > 1) %>% pull(row) %>% unique()
  
  save(lv.trian, file =  '../private/temp/lv.trian.RData')
  print(paste0("compressie: ",  100 * (1 - round(length(veld.id.voronoi) / nrow(lv.sub ), 2)), "%"))
  
  vor_spdf <- voronoi_polygon(data = lv.trian %>% filter(row %in% veld.id.voronoi))
  vor_spdf@proj4string <- CRS(amsf$prj4)
  
  poly <- st_as_sf(vor_spdf) %>%
    group_by(veld.id) %>% 
    summarize() %>%
    mutate()
  
  lv.sub <- lv.sub %>% ungroup() %>%
    mutate(ref.veld = over(
      as( lv.sub %>% st_as_sf(., coords = c('x', 'y'), crs = amsf$prj4) %>% st_transform(amsf$code),
          "Spatial"),
      as( 
        poly %>% st_as_sf(., coords = c('x', 'y'), crs = amsf$prj4) %>% st_transform(amsf$code), "Spatial")) %>%
        pull(veld.id))
  
  
  print(paste("aantal fouten: ", nrow(lv.sub %>% ungroup() %>% filter(veld.id != ref.veld)
  )))
  
  return(poly)
}

