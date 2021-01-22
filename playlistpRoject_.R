#getting the needed libaries
library(tidyverse) #everything
library(kableExtra) #makes tables print pretty
library(spotifyr) #also, everything
library(lubridate)
library(dplyr) #also, everything
library(batchtools)

#setting the authorization
Sys.setenv(SPOTIFY_CLIENT_ID = 'your client ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your client secret')
access_token <- get_spotify_access_token()

#using spotify r to get all my playlists

#can analyze up to past 50 playlists. which works because i made 41 playlists  
#in 2020
playlists <- get_my_playlists(limit=50)

#removing playlists i don't own
playlists <- playlists %>% filter(owner.id == 'isabella_f')
#getting the past 41 playlists, for each of the ones i made in 2020
playlists <- playlists %>% head(playlists, n=41)
#sanity cross check to make sure all is good, and the df is all my 2020 playlists
#i also exported the playlist ids at this point in case i don't finish this
#project by the time it takes me to make 9 more playlists on spotify,
#therby making some playlists harder to access. thinking ahead, you know?
head(playlists,1)
tail(playlists,1)

#I'm writing the URIs to a csv just in case I need to access tha playlists in another
#way. I would just use these instead of the get_my_playlist function, but
#that function seems to be the best from spotifyR regarding what I want to do
write.csv(playlists$uri, "2020playlistURI.csv")

#those are saved and i am safe!

#dope! 

#first, i am going to add a dataframe of the tracks to my playlists dataframe

tracks = sapply(playlists$id,get_playlist_tracks )
playlists$tracks <- tracks

#disclaimer that i can't believe that worked but rock on


#random spot check to make sure all is well. looks like i did the CMBYN score
#as a playlist here. oops.
playlists$tracks[[22]]

# realized that some of my playlists have like 1 or 0 songs on them and i just 
# made them because i thought the name was fun but then i felt uninspired soon after...
# i figured at least 2 songs is worth analyzing. cool

playlists <- playlists %>% filter(tracks.total >=2)

#now i have 38 rockin playlists. whether they actually rock or not is up to you

#In order to properly do group bys for each playlist when analyzing the
#tracks, I need to add the playlist id or uri to each row of the tracks DF!
#i'm doing this with a for loop i'm so sorry

for(i in (1:38)){
  pid <- playlists$id[i]
  p.name <- playlists$name[i]
  playlists$tracks[[i]]$pid <- pid
  playlists$tracks[[i]]$pname <- p.name
}


#spot checking to make sure that worked
playlists$tracks[[22]]
playlists$tracks[[21]]


#Because analyzing tracks can only be done 100 tracks att a time, I am creating
#new dataframes for each playlist that contain all the track ids contained in 
#that playlist. That way, it will be much much easier to analyze the
#details of the tracks on each individual playlist

track_playlists = playlists %>% select(tracks) %>%  unnest(cols = 'tracks')  %>% select(track.id,pid, pname) 

unique.pids = distinct(track_playlists, pid,pname)


playlist.dfs <- list()

#inspired by https://stackoverflow.com/questions/34832171/using-a-loop-to-create-multiple-data-frames-in-r
for(i in 1:length(unique.pids$pid)){
  tmp.df<- track_playlists %>% filter(track_playlists$pid == unique.pids$pid[i])
  names(tmp.df) <- unique.pids$pname[i]
  colnames(tmp.df) = c('trackid','pid','pname')
  playlist.dfs[[i]] <- tmp.df
} 


#spot check to make sure the correct tracks are in each playlist
playlist.dfs[38]

#Now i can start to analyze the audio features for each playlist!
#I want to add the audio featurs of each track to my dataframes of the playlist
#and track ids.
for(i in (1:38)){
  track_ids <- playlist.dfs[[i]]$trackid
  t = get_track_audio_features(track_ids)
  playlist.dfs[[i]] = cbind(playlist.dfs[[i]],t)
}


#going to try to make this one huge data frame but honestly that might fail.

all.playlists = playlist.dfs[[1]]
for(i in (2:38)){
  all.playlists = rbind(all.playlists,playlist.dfs[[i]])
}
#all.playlists

#worked! so dope! now i have a df of each track, the playlist, and some of 
#the info i need to answer some of my first questions. 


#my coffee pour ocean shore rowboat oar playlist has some NA rows 
#because i added songs from my computer. those happened to be iris which was
#released by phoebe bridgers and maggie rogers and the song that 
#timothee chalamet played on the bass in the film Lady Bird (2017). yep. 
#so i'm going to remove those

all.playlists = all.playlists[complete.cases(all.playlists), ]
nrow(all.playlists)

#that removed the two songs!

#luckily, the names are unique too so i don't have to group by pid
#i'm just so used to SQL and grouping by that. but this is my own data so. 
#rock on! arrange function taken from tayloR analysis by Simran Vasta
all.playlists %>% group_by(pname) %>% summarise(mean(valence)) %>% 
  arrange(dplyr::desc(`mean(valence)`)) %>%  #tail(5) %>%
  kable(.,col.names = c('Playlist', 'Mean Valence'))  %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(row = 1:5, background = "#181818", color = "#15b358") #%>% save_kable(., 'playlistpRoject/q1tail.png')

#Question 1 can be answered with the table above! now onto Q2, "dancability"
all.playlists %>% group_by(pname) %>% summarise(mean(danceability)) %>% 
  arrange(dplyr::desc(`mean(danceability)`)) %>% head(5) %>%
  kable(.,col.names = c('Playlist', 'Mean Danceability'))  %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T) %>%
  row_spec(row = 1:5, background = "#181818", color = "#15b358") %>% save_kable(., 'playlistpRoject/q2head.png')

# Question 3. Which of my playlists is the most cohesive? This question
#is a bit different. I decided to measuere this by using the range of valence
#so, which playlist is the most alike in therms of positivity / negativity. 

Range <- function(max, min){
  abs(max - min)
}

all.playlists %>% group_by(pname) %>% summarise(Range(max(valence),min(valence))) %>% 
  arrange((`Range(max(valence), min(valence))`)) %>% head(5) %>%
  kable(.,col.names = c('Playlist', 'Range of Valence'))  %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(row = 1:5, background = "#181818", color = "#15b358")

#question 4 can also be answered using the same logic as 1 and 2
all.playlists %>% group_by(pname) %>% summarise(mean(energy)) %>% 
  arrange(dplyr::desc(`mean(energy)`))  %>% #tail(5) %>%
  kable(.,col.names = c('Playlist', 'Mean Energy'))  %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(row = 1:5, background = "#181818", color = "#15b358")#

#Question 5 asks for the most positive and most negative songs I put on a playlist.
#Luckily with my dataframe I just need to find the rows with the maximum and minimum 
#valence

mos.pos = all.playlists %>% filter(valence == max(valence)) %>% 
  select(trackid)%>% get_track() 


kable(data.frame(mos.pos$name,mos.pos$artist$name), 
      col.names = c('Song','Artist'), caption = 'Most Positive Song on a 2020 Playlist') %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(row = 1, background = "#181818", color = "#15b358")

mos.neg = all.playlists %>% filter(valence == min(valence)) %>% 
  select(trackid) %>% get_track()

kable(data.frame(mos.neg$name,mos.neg$artist$name), 
      col.names = c('Song','Artist'), caption = 'Most Negative Song on a 2020 Playlist') %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(row = 1, background = "#181818", color = "#15b358") 

#I'm also curious about the most negative song that isn't on the playlist 
#coming of age tunnel:work edition since that's my playlist with filmscores 
#and that music is very different than what i listen too when i'm not working!

min.not.film = all.playlists %>% filter(pname != 'coming of age tunnel: work edition' & pname != 'do u know') %>% 
  select(valence) %>% min()
#the same track is on two playlists, hence the head(1)
neg.not.film = all.playlists %>% filter(valence == min.not.film) %>% 
  select(trackid) %>% head(1) %>% get_track()

kable(data.frame(neg.not.film$name,neg.not.film$artist$name[2]), 
      col.names = c('Song','Artist'),
      caption = 'Most Negative Song on a 2020 Non-Film Score Playlist') %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(row = 1, background = "#181818", color = "#15b358")



#question 6 is the song that appeared across the most playlist, should be 
#pretty simple

#mode function taken from https://stackoverflow.com/questions/2547402/how-to-
#find-the-statistical-mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

most_common = get_track(Mode(all.playlists$trackid))

kable(data.frame(most_common$name,most_common$artist$name), 
      col.names = c('Song','Artist'),
      caption = 'Most Common Song on my 2020 Playlists') %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(row = 1, background = "#181818", color = "#15b358")



#One questiton left!
#I LOVE THREE QUARTER TIME! Which of my songs were written in waltz, finally 
#letting me make the waltz playlists I've always wanted to make

waltz = all.playlists %>% filter(time_signature == 3)
w.ids = waltz$trackid

w.name = list()
w.artist = list()

for(i in (1:length(w.ids))){
  w = get_track(w.ids[i])
  w.name = append(w.name,w$name)
  w.artist = append(w.artist,w$artist$name[1])
  
}


waltz.playlist = cbind(data.frame(song = unlist(w.name)),
                       data.frame(artist = unlist(w.artist)))

waltz.playlist %>% filter(artist!='Christian Borle' & song != 'The Ball' & song != 
                            'It All Fades Away' & song!= 'Stay'  & song != 'My Petersburg') %>% distinct %>% arrange(song)  %>% kable(., caption = 'Songs Written in Waltz')%>%  
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T) %>%
  row_spec(row = 1:57, background = "#181818", color = "#15b358") 









