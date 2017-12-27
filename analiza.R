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



# liczba postów publikowanych dzień po dniu
fb_posts %>%
   filter(year(created_time) == 2017) %>%
   mutate(day = date(created_time)) %>%
   count(from_name, day) %>%
   ggplot() +
   geom_point(aes(day, n, color = from_name), alpha = 0.5) +
   geom_smooth(aes(day, n, fill = from_name, color = from_name), se = FALSE, show.legend = FALSE) +
   labs(title = "Liczba postów publikowanych na fanpage seriali, dzień po dniu",
        x = "Data", y = "Liczba postów", color = "") +
   theme(legend.position = "bottom")


# liczba postów publikowana w godzinach/tydzień
fb_posts %>%
   filter(year(created_time) == 2017) %>%
   by(.$from_name,
      function(x) {
         x %>%
            count(created_weekday, created_hour) %>%
            ggplot() +
            geom_tile(aes(created_weekday, created_hour, fill = n), color = "gray50") +
            scale_y_continuous(breaks = seq(0, 23, 3), trans = "reverse") +
            scale_fill_distiller(palette = "RdYlGn") +
            labs(title = unique(x$from_name),
                 x = "Dzień tygodnia", y = "Godzina", fill = "Liczba postów") +
            theme(legend.position = "bottom")
      }
   )


# liczba komentarzy publikowanych dzień po dniu
fb_data %>%
   filter(year(post_created_time) == 2017) %>%
   mutate(day = date(comment_created_time)) %>%
   count(post_from_name, day) %>%
   ggplot() +
   geom_point(aes(day, n, color = post_from_name), alpha = 0.5) +
   geom_smooth(aes(day, n, fill = post_from_name, color = post_from_name), se = FALSE, show.legend = FALSE) +
   scale_y_log10() +
   labs(title = "Liczba komentarzy publikowanych na fanpage seriali, dzień po dniu",
        x = "Data", y = "Liczba komentarzy (skala logarytmiczna)", color = "") +
   theme(legend.position = "bottom")



# liczba komentarzy/post publikowanych dzień po dniu
fb_posts %>%
   filter(year(created_time) == 2017) %>%
   mutate(day = date(created_time)) %>%
   group_by(from_name, day) %>%
   summarise(p = sum(comments_count)/n()) %>%
   ungroup() %>%
   ggplot() +
   geom_point(aes(day, p, color = from_name), size = 0.7, alpha = 0.5) +
   geom_smooth(aes(day, p, color = from_name, fill = from_name), se = FALSE, show.legend = FALSE) +
   labs(title = "Liczba komentarzy/post publikowanych na fanpage seriali, dzień po dniu",
        x = "Data", y = "Liczba komentarzy/post", color = "") +
   theme(legend.position = "bottom")



# aktywność na stronie w ciągu tygodnia
fb_posts %>%
   filter(year(created_time) == 2017) %>%
   by(.$from_name,
      function(x) {
         mean_actions <- mean(x$comments_count+x$likes_count, na.rm = TRUE)

         x %>%
            rowwise() %>%
            mutate(s = sum(comments_count+likes_count, na.rm = TRUE)) %>%
            ungroup() %>%
            filter(s <= mean(s) + 2 * sd(s)) %>%
            group_by(created_weekday, created_hour) %>%
            # to ciekawy wskaźnik
            summarise(t = sum(comments_count+likes_count, na.rm = TRUE)/mean_actions) %>%
            ungroup() %>%
            ggplot() +
            geom_tile(aes(created_weekday, created_hour, fill = t), color = "gray80") +
            scale_y_continuous(breaks = seq(0, 23, 3), trans = "reverse") +
            scale_fill_distiller(palette = "RdYlGn") +
            labs(title = paste0("Aktywność* komentujących posty na stronie serialu ", unique(x$from_name)),
                 subtitle = "Aktywność = suma komentarzy podzielona przez ich średnią",
                 x = "Dzień tygodnia publikacji postu", y = "Godzina publikacji postu",
                 fill = "Aktywność") +
            theme(legend.position = "bottom")

      }
   )



# jak szybko pojawiaja sie komentarze?
fb_data %>%
   mutate(delta_time = as.numeric(comment_created_time - post_created_time)/3600) %>%
   filter(delta_time > 0) %>%
   ggplot() +
   geom_density(aes(delta_time, fill = post_from_name), alpha = 0.5) +
   scale_x_continuous(breaks = c(1/60, 5/60, 1/4, 1/2, 1, 2, 3, 6, 12, 24, 2*24, 5*24, 10*24, 30*24),
                      labels = c("1m", "5m", "15m", "30m", "1h", "2h", "3h", "6h", "12h", "1d", "2d", "5d", "10d", "30d"),
                      trans = "log10") +
   labs(title = "Czas jaki mija od publikacji posta do publikacji komentarza",
        x = "", y = "Gestość prawdopodobieństwa", fill = "Fanpage") +
   theme(legend.position = "bottom")


# komcie w Belfrze
fb_data %>%
   mutate(delta_time = as.numeric(comment_created_time - post_created_time)/3600) %>%
   filter(post_from_name == "Belfer") %>%
   group_by(post_id, post_created_time) %>%
   summarize(m_delta_time = mean(delta_time)) %>%
   ungroup() %>%
   top_n(10, m_delta_time) %>%
   left_join(fb_posts %>% select(id, message), by = c("post_id"="id")) %>%
   select(-post_id) %>%
   arrange(desc(m_delta_time))


# który post żył tak długo?
fb_data %>%
   mutate(delta_time = as.numeric(comment_created_time - post_created_time)/3600) %>%
   filter(post_from_name == "Belfer") %>%
   group_by(post_id, post_created_time) %>%
   summarize(m_delta_time = mean(delta_time)) %>%
   ungroup() %>%
   top_n(1, m_delta_time) %>%
   select(post_id) %>%
   left_join(fb_data, by = "post_id") %>%
   select(post_created_time, comment_created_time, comment_message)


# komcie - w którym dniu tygodnia i o jakiej godzine pojawiają się komentarze
fb_data %>%
   mutate(comment_created_hour = hour(comment_created_time),
          comment_created_weekday = wday(comment_created_time, label = TRUE) %>%
             factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                    labels = c("Pn", "Wt", "Śr", "Cz", "Pt", "Sb", "Nd"))) %>%
   by(.$post_from_name,
      function(x) {
         x %>%
            count(comment_created_weekday, comment_created_hour) %>%
            ungroup() %>%
            ggplot() +
            geom_tile(aes(comment_created_weekday, comment_created_hour, fill = n), color = "gray80", show.legend = FALSE) +
            scale_fill_distiller(palette = "RdYlGn") +
            scale_y_reverse() +
            labs(title = paste(unique(x$post_from_name), "- kiedy pojawiaja sie komentarze?"),
                 x = "", y = "Godzina")
      }
   )




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
   geom_tile(aes(from_name, word, fill = n), color = "gray50", show.legend = FALSE) +
   scale_fill_distiller(palette = "RdYlGn") +
   labs(title = "Najpopularniejsze słowa w postach",
        x = "", y = "")



# rozbicie komentarzy na słowa
fb_comments_messages <- fb_comments %>%
   select(post_from_name, post_id, comment_message) %>%
   unnest_tokens(word, comment_message, token = "words", drop = FALSE) %>%
   select(-comment_message) %>%
   filter(!is.na(word)) %>%
   filter(!word %in% pl_stop_words) %>%
   filter(is.na(as.numeric(word))) %>%
   filter(nchar(word) > 2) %>%
   filter(!word %in% c("czekam", "doczekać", "serial", "odcinek", "mogę", "sezon", "już", "się", "pozdrawiam",
                       "czekamy", "super", "sie", "serialem", "serialu", "premiera", "juz", "odcinka", "oglądam",
                       "odcinki", "sezonu", "sezonie", "najlepszy", "gratulacje", "obejrzeć", "niecierpliwością",
                       "odcinków", "oglądać", "odcinku", "czekać"))

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
   geom_tile(aes(post_from_name, word, fill = n), color = "gray50") +
   scale_fill_distiller(palette = "RdYlGn") +
   labs(title = "Najpopularniejsze słowa w komentarzach",
        x = "", y = "")


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
   ungroup() %>%
   by(.$post_from_name,
      function(x) {
         x %>%
            top_n(20, p) %>%
            arrange(p) %>%
            mutate(word = fct_inorder(word)) %>%
            ggplot() +
            geom_col(aes(word, p), fill = "lightgreen", color = "gray50") +
            coord_flip() +
            labs(title = paste(unique(x$post_from_name), "- najpopularniejsze słowa w komentarzach"),
                 x = "", y = "")
      }
   )


# kto komentuje na wszystkich serialach?
fb_data %>%
   count(post_from_name, comments_from_name, sort = TRUE) %>%
   ungroup() %>%
   spread(post_from_name, n) %>%
   rowwise() %>%
   mutate(comm_sum = sum(Belfer, `Diagnoza TVN`, Wataha)) %>%
   ungroup() %>%
   filter(!is.na(comm_sum)) %>%
   arrange(comm_sum) %>%
   mutate(comments_from_name = fct_inorder(comments_from_name)) %>%
   select(-comm_sum) %>%
   gather(serial, val, -comments_from_name) %>%
   ggplot() +
   geom_tile(aes(serial, comments_from_name, fill = val), color = "gray50", show.legend = FALSE) +
   scale_fill_distiller(palette = "RdYlGn") +
   labs(title = "Komentujcacy wszystkie seriale", x = "", y = "")



# komentarze na bigramy
fb_comments_messages_bigrams <- fb_comments %>%
   select(post_from_name, comment_message) %>%
   unnest_tokens(word, comment_message, token = "ngrams", n = 2, drop = FALSE) %>%
   select(-comment_message) %>%
   separate(word, c("word1", "word2"), sep = " ") %>%
   filter(!word1 %in% pl_stop_words) %>%
   filter(!word1 %in% c("czekam", "doczekać", "serial", "odcinek", "mogę", "sezon", "już", "się", "pozdrawiam",
                       "czekamy", "super", "sie", "serialem", "serialu", "premiera", "juz", "odcinka", "oglądam",
                       "odcinki", "sezonu", "sezonie", "najlepszy", "gratulacje", "obejrzeć", "niecierpliwością",
                       "odcinków", "oglądać", "odcinku", "czekać")) %>%
   filter(is.na(as.numeric(word1))) %>%
   filter(nchar(word1) > 2) %>%

   filter(!word2 %in% pl_stop_words) %>%
   filter(!word2 %in% c("czekam", "doczekać", "serial", "odcinek", "mogę", "sezon", "już", "się", "pozdrawiam",
                       "czekamy", "super", "sie", "serialem", "serialu", "premiera", "juz", "odcinka", "oglądam",
                       "odcinki", "sezonu", "sezonie", "najlepszy", "gratulacje", "obejrzeć", "niecierpliwością",
                       "odcinków", "oglądać", "odcinku", "czekać")) %>%
   filter(is.na(as.numeric(word2))) %>%
   filter(nchar(word2) > 2) %>%
   mutate(word = paste(word1, word2, sep = " ")) %>%
   select(-word1, -word2)


fb_comments_messages_bigrams %>%
   count(post_from_name, word) %>%
   ungroup() %>%
   by(.$post_from_name,
      function(x) {
         x %>%
            top_n(30, n) %>%
            arrange(n) %>%
            mutate(word = fct_inorder(word)) %>%
            ggplot() +
            geom_col(aes(word, n), fill = "lightgreen", color = "gray50") +
            coord_flip() +
            labs(title = paste(unique(x$post_from_name), "- najpopularniejsze bigramy w komentarzach"), x = "", y = "")
      }
   )
