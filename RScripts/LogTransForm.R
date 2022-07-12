# Log transformation possibiity

df <-  read_csv("spotify_data - spotify_data.csv")
df %>% 
  ggplot(aes(x = danceability)) +
  geom_histogram()
df %>% 
  ggplot(aes(x = energy)) +
  geom_histogram()
df %>% 
  ggplot(aes(x = key)) +
  geom_histogram()
df %>% 
  ggplot(aes(x = log(-(loudness)))) +
  geom_histogram()
df %>% 
  ggplot(aes(x = loudness)) +
  geom_histogram()
df %>% 
  ggplot(aes(x = mode)) +
  geom_histogram()

df %>% 
  ggplot(aes(x = log(speechiness)))+
  geom_histogram()
df %>% 
  ggplot(aes(x = log(acousticness))) +
  geom_histogram()
# Just turn it to a dummy honestly
df %>% 
  mutate(instrDummy = ifelse(instrumentalness == 0 ,1,0)) %>% 
  ggplot(aes(x = instrDummy)) +
  geom_bar()
df %>% 
  ggplot(aes(log(instrumentalness))) +
  geom_histogram()
df %>% 
  ggplot(aes(x = log(liveness))) +
  geom_histogram()
df %>% 
  ggplot(aes(x = valence)) +
  geom_histogram()
df %>% 
  ggplot(aes(x = log(duration_ms))) +
  geom_histogram()
# Omit this variable probably
df %>% 
  mutate(timeDummy = time_signature == 4) %>% 
  ggplot(aes(x = timeDummy)) +
  geom_bar()
# Impute the median honestly, 0 makes no sense
df %>% 
  ggplot(aes(x = log(chorus_hit + 1))) +
  geom_histogram()
df %>% 
  ggplot(aes(chorus_hit)) +
  geom_boxplot()
#
df %>% 
  ggplot(aes(log(sections + 1))) +
  geom_histogram()
df %>% 
  filter(SumRank != 0) %>% 
  ggplot(aes(log(SumRank))) +
  geom_histogram()
