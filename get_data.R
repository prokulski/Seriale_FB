rm(list = ls())

n_posts <- 2000
lista_stron <- c("diagnozatvn", "BELFERCanalPlusPolska", "WatahaHBO")

library(Rfacebook)
library(tidyverse)
library(lubridate)
library(tidytext)
library(forcats)

load("fb_oauth.rda")

get_fanpage_posts <- function(fb_page_name, n = 100, token) {
   # najnowsze posty z fanpage
   fb_page_posts <- getPage(fb_page_name, token = token, n = n)

   # bierzemy to co nam potrzebne
   fb_page_posts <- fb_page_posts %>% select(-story,-link)

   return(fb_page_posts)
}

get_post_comments <- function(post_id, token, n = 5000) {
   # dane o poście - komcie i liczba like'ów
   post <- getPost(post_id, token = token, n = n)

   # czy są komentarze?
   if (!is.null(post$comments) && nrow(post$comments) != 0) {

      # bierzemy interesujace nas dane o komentarzach
      post_comments <- post$comments %>%
         select(comment_from_id = from_id,
                comments_from_name = from_name,
                comment_created_time = created_time,
                comment_message = message)

      # dodajemy info o poście, do którego są komentarze
      post_comments$post_id <- post$post$id
      post_comments$post_from_id <- post$post$from_id
      post_comments$post_from_name <- post$post$from_name

      return(post_comments)
   }

   return(data_frame())
}

#####
# pobranie postów
fb_posts <- lista_stron %>% map_df(get_fanpage_posts, n_posts, fb_oauth)

# pobranie komentarzy do postów
fb_comments <- fb_posts$id %>% map_df(get_post_comments, fb_oauth, max(fb_posts$comments_count))


# poprawki w datach
## posty
fb_posts <- fb_posts %>%
   mutate(created_time = ymd_hms(created_time) %>%
             with_tz(tzone = "Europe/Warsaw")) %>%
   mutate(created_hour = hour(created_time),
          created_weekday = wday(created_time, label = TRUE) %>%
             factor(levels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
                    labels = c("Pn", "Wt", "Śr", "Cz", "Pt", "Sb", "Nd")),
          created_weeknum = isoweek(created_time))


# zapisanie danych na później
saveRDS(fb_posts, file = "fb_posts.RDS")
saveRDS(fb_comments, file = "fb_comments.RDS")
