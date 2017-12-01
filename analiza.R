rm(list = ls())

library(tidyverse)
library(lubridate)
library(tidytext)
library(forcats)


fb_posts <- readRDS("fb_posts.RDS")
fb_comments <- readRDS("fb_comments.RDS")


# łączymy obie tabele
fb_data <- left_join(fb_comments,
                     fb_posts %>% select(id, created_time),
                     by = c("post_id" = "id")) %>%
   rename(post_created_time = created_time) %>%
   mutate(comment_created_time = ymd_hms(comment_created_time) %>% with_tz(tzone = "Europe/Warsaw")) %>%
   mutate(post_created_time = ymd_hms(post_created_time) %>% with_tz(tzone = "Europe/Warsaw"))




# aktywność na stronie w ciągu tygodnia
fb_posts %>%
   # tylko posty opublikowane w 2017 roku
   filter(year(created_time) == 2017) %>%
   group_by(from_name, created_weekday, created_hour) %>%
   # to ciekawy wskaźnik
   summarise(t = sum(comments_count+likes_count)/mean(comments_count+likes_count)) %>%
   ungroup() %>%
   filter(!is.nan(t)) %>%
   # filter(t > quantile(t, 0.75)) %>%
   ggplot() +
   geom_tile(aes(created_weekday, created_hour, fill = t), color = "gray80") +
   facet_wrap(~from_name, ncol=1) +
   scale_y_continuous(breaks = seq(0, 23, 3), trans = "reverse") +
   scale_fill_distiller(palette = "RdYlGn") +
   labs(title = "Aktywność* komentujących posty na stronie serialu",
        subtitle = "Aktywność = suma komentarzy podzielona przez ich średnią",
        x = "Dzień tygodnia publikacji postu", y = "Godzina publikacji postu")



# jak szybko pojawiaja sie komentarze?
fb_data %>%
   mutate(delta_time = as.numeric(comment_created_time - post_created_time)/3600) %>%
   filter(delta_time > 0) %>%
   ggplot() +
   geom_density(aes(delta_time, fill = post_from_name), alpha = 0.2) +
   scale_x_continuous(breaks = c(1/60, 5/60, 1/6, 1/4, 1/2, 1, 2, 3, 4, 5, 6, 12, 18, 24, 48, 120, 240, 720),
                      labels = c("1m", "5m", "10m", "15m", "30m", "1h", "2h", "3h", "4h", "5h", "6h", "12h", "18h", "1d", "2d", "5d", "10d", "30d"),
                      trans = "log10")



# komcie - w którym dniu tygodnia i o jakiej godzine pojawiają się komentarze
fb_data %>%
   mutate(comment_created_hour = hour(comment_created_time),
          comment_created_weekday = wday(comment_created_time, label = TRUE, week_start = 1) %>%
             factor(labels = c("Pn", "Wt", "Śr", "Cz", "Pt", "Sb", "Nd"))) %>%
   count(post_from_name, comment_created_weekday, comment_created_hour) %>%
   ungroup() %>%
   # tylko gó○rna ćwiartka per strona
   group_by(post_from_name) %>%
   filter(n > quantile(n, 0.9)) %>%
   ungroup() %>%
   ggplot() +
   geom_tile(aes(comment_created_weekday, comment_created_hour, fill = n)) +
   facet_wrap(~post_from_name, ncol = 1) +
   scale_fill_distiller(palette = "RdYlGn") +
   scale_y_reverse()




# teksty
pl_stop_words <- read_lines("../!polimorfologik/polish_stopwords.txt")
pl_stop_words <- c(pl_stop_words,
                   "canal", "http", "https", "www.hbo.pl", "hbo", "diagnoza.tvn.pl", "html", "youtu.be", "bit.ly", "www.wirtualnemedia.pl", "magicznebieszczady")

# rozbicie postów na słowa
fb_posts_messages <- fb_posts %>%
   select(from_name, message) %>%
   unnest_tokens(word, message, token = "words", drop = FALSE) %>%
   select(-message) %>%
   filter(!is.na(word)) %>%
   filter(!word %in% pl_stop_words) %>%
   filter(is.na(as.numeric(word))) %>%
   filter(nchar(word) > 2)

# najpopularniejsze słowa w postach wg fanpage
fb_posts_messages %>%
   count(from_name, word) %>%
   ungroup() %>%
   group_by(from_name) %>%
   top_n(20, n) %>%
   ungroup() %>%
   arrange(desc(word)) %>%
   mutate(word = fct_inorder(word)) %>%
   ggplot() +
   geom_tile(aes(from_name, word, fill = n)) +
   scale_fill_distiller(palette = "RdYlGn")



# rozbicie komentarzy na słowa
fb_comments_messages <- fb_comments %>%
   select(post_from_name, post_id, comment_message) %>%
   unnest_tokens(word, comment_message, token = "words", drop = FALSE) %>%
   select(-comment_message) %>%
   filter(!is.na(word)) %>%
   filter(!word %in% pl_stop_words) %>%
   filter(is.na(as.numeric(word))) %>%
   filter(nchar(word) > 2)

# najpopularniejsze słowa w komentarzach wg fanpage
fb_comments_messages %>%
   count(post_from_name, word) %>%
   ungroup() %>%
   group_by(post_from_name) %>%
   top_n(20, n) %>%
   ungroup() %>%
   arrange(desc(word)) %>%
   mutate(word = fct_inorder(word)) %>%
   ggplot() +
   geom_tile(aes(post_from_name, word, fill = n)) +
   scale_fill_distiller(palette = "RdYlGn")


fb_comments_messages %>%
   # ile różnych słóW występuje przy komentarzach do konkretnego posta?
   group_by(post_id) %>%
   mutate(n_words_in_posts = n()) %>%
   ungroup() %>%
   # ile jest różnych słów w komciach dla danej strony?
   group_by(post_from_name, word) %>%
   mutate(n_times_word = n()) %>%
   ungroup() %>%
   # p = popularność słowa na stronie / ile słów przy komciu
   mutate(p = n_times_word/n_words_in_posts) %>%
   # średnia wartość p dla strony i słowa
   group_by(post_from_name, word) %>%
   summarise(p = mean(p)) %>%
   top_n(20, p) %>%
   ungroup() %>%
   ggplot() +
   geom_tile(aes(post_from_name, word, fill = p)) +
   scale_fill_distiller(palette = "RdYlGn")
