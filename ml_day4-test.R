raw_park <- readLines("speech_park.txt",encoding = 'UTF-8')




park <- raw_park %>% 
  str_replace_all("[^가-힣]"," ") %>% 
  str_squish() %>% 
  as_tibble()

park_noun <- park %>% 
  unnest_tokens(input=value,
                output=word,
                token=extractNoun)

park_noun <- park_noun %>% 
  count(word, sort=T) %>% 
  filter(str_count(word)>1)


parktop20 <- park_noun %>% 
  head(20)




ggplot(parktop20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))



ggplot(park_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(3, NA),
               range = c(3, 15)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()




##########################################



raw_ham <- readLines("data.txt")
raw_ham <- raw_ham %>% 
  str_replace_all("[^a-zA-z]"," ") %>% 
  str_squish() %>% 
  as_tibble()


ham <- raw_ham %>% 
  unnest_tokens(input=value,
                output=word,
                token="words")


ham <- ham %>% 
  count(word, sort=T) %>% 
  filter(str_count(word)>1)

hamtop20 <- ham %>% 
  head(20)

ggplot(hamtop20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
