#install required packages
if(!require(pacman))install.packages("pacman")
pacman::p_load('rvest', 'stringi', 'dplyr', 'tidyr', 'measurements', 'reshape2','foreach','doParallel','raster','curl','httr','Iso','stringi','janitor')


#set up the link to PGA website
PGA_url <- "https://www.pgatour.com"
base_url <- "https://www.pgatour.com/players/"

#extract URL for players
pga_web=read_html(paste0(PGA_url,'/players.html'))
plyers_link <- pga_web%>%html_nodes("[class='player-card']")%>%html_nodes('a')%>%html_attr('href')
url_link_players<-paste0(PGA_url,plyers_link)

#extract info of each player
profile_detail<-function(links)
{
  players_info <- url_link_players[links] %>%
    read_html() %>%
    html_nodes('div.s-col') %>%
    html_text() %>%
    gsub('\\h+', ' ', ., perl = TRUE) %>% as.character()%>% stri_split_regex("\n \n \n")%>%lapply(function(x){x[!is.na(x) & x!=""]})%>%unlist()
  return(players_info)
}  

#setup parallel backend to use many processors to extract all tables from url links above (url_link_players)
cores=detectCores()
cl <- makeCluster(cores)
clusterExport(cl,c("%>%","read_html","html_nodes","html_table","plyers_link","url_link_players","html_text","stri_split_regex"))
system.time(players_profile<-parLapply(cl,1:length(url_link_players),profile_detail))
stopCluster(cl)

#split the strings using regex and transpose the rows
players_info_cleaned<-function(player_info_no)
  
{
    players_info <- stringr::str_match(players_profile[[player_info_no]], '(?:.*\n)?\\s(.*)\n\\s(.*)')[, -1]%>%data.frame()%>%rev()%>%t()%>%row_to_names(row_number = 1)%>%data.frame()
}

cleaned_players_info<-lapply(1:length(players_profile), players_info_cleaned)

#combining the list of data frames to obtain one big dataframe 
players_final_data<-data.table::rbindlist(cleaned_players_info,fill = TRUE)

#write the dataframe in csv
write.csv(players_final_data,"C:/Users/SurafelTilahun/players_info.csv")

