library(tidyverse)
library(rchess)
library(patchwork)
library(scico)
library(camcorder)
library(showtext)


# Set fonts
font_add_google("Fira Sans Extra Condensed","cond")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 300 
)

# Load data
chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

# Extract pieces moves
# Based on this script by Cormac Monaghan :
# https://github.com/C-Monaghan/tidytuesday/tree/main/2024/2024-09-23

convert_to_pgn <- function(moves, game_id) {
  
  move_list <- strsplit(moves, " ")[[1]]
  
  # Generate the PGN format by numbering the moves
  pgn <- ""
  
  for (i in seq(1, length(move_list), by = 2)) {
    move_number <- (i + 1) / 2
    if (i < length(move_list)) {
      pgn <- paste0(pgn, move_number, ". ", move_list[i], " ", move_list[i+1], " ")
    } else {
      pgn <- paste0(pgn, move_number, ". ", move_list[i])
    }
  }
  
  # Returning pgn string
  return(pgn)
}

# Processing moves (rchess package) 
process_moves <- function(p) {
  chss <- Chess$new()
  chss$load_pgn(p)
  chss$history_detail()
}

# Converting moves into pgn format
chess_games <- chess %>%
  select(game_id, moves) %>%
  mutate(
    game_id_origin=game_id,
    game_id = seq(1:nrow(chess)),
    moves = mapply(convert_to_pgn, moves, game_id)) # pgn conversion function

# Converting to game history
tst<-bind_cols(
  game_id=chess_games$game_id[1],
  process_moves(chess_games$moves[1])
)

# Extract move lists for each game
for (i in 2:length(chess_games$moves)){
  temp<-bind_cols(
    game_id=chess_games$game_id[i],
    process_moves(chess_games$moves[i])
  )
  tst<-tst%>%
    bind_rows(temp)
  if (i %in% seq(1,20000,500)){
    write_csv(tst,"data.csv")
  } else {}
  print(i)
}

# Re-read data
# data<-read_csv('data.csv')

# Put all cells in the same column
clean<-data%>%
  select(game_id_new=game_id,piece,from,to)%>%
  pivot_longer(!c(game_id_new,piece), names_to = "move", values_to = "cell")%>%
  drop_na(cell)

# Bring back the original data based on game_id to differentiate
# between losing/winning games
tab_conv <- chess_games%>%
  select(game_id_new=game_id,game_id=game_id_origin)%>%
  left_join(chess,relationship = "many-to-many")

clean<-clean%>%
  left_join(tab_conv,relationship = "many-to-many")

# Split cell between col and rows
clean[c("col", "row")]=stringr::str_split(clean$cell,'',simplify=T)

# Convert to numeric and add some jitter for "fancier" kernel maps
clean_num <- clean%>%
  mutate(col_num=case_when(
    col=="a"~1,
    col=="b"~2,
    col=="c"~3,
    col=="d"~4,
    col=="e"~5,
    col=="f"~6,
    col=="g"~7,
    col=="h"~8
  ))%>%
  mutate(
    col_jitter=jitter(col_num, amount=0.5),
    row_jitter=jitter(as.numeric(row), amount=0.5)
  )

# Filter results for all white pieces
wh_pawn_win<-clean_num%>%
  filter(str_detect(piece,"Pawn")&str_detect(piece,"2")&winner=="white")

wh_pawn_lose<-clean_num%>%
  filter(str_detect(piece,"Pawn")&str_detect(piece,"2")&winner=="black")

wh_rook_win<-clean_num%>%
  filter(str_detect(piece,"Rook")&str_detect(piece,"1")&winner=="white")

wh_rook_lose<-clean_num%>%
  filter(str_detect(piece,"Rook")&str_detect(piece,"1")&winner=="black")

wh_bishop_win<-clean_num%>%
  filter(str_detect(piece,"Bishop")&str_detect(piece,"1")&winner=="white")

wh_bishop_lose<-clean_num%>%
  filter(str_detect(piece,"Bishop")&str_detect(piece,"1")&winner=="black")

wh_knight_win<-clean_num%>%
  filter(str_detect(piece,"Knight")&str_detect(piece,"1")&winner=="white")

wh_knight_lose<-clean_num%>%
  filter(str_detect(piece,"Knight")&str_detect(piece,"1")&winner=="black")

wh_queen_win<-clean_num%>%
  filter(piece=="White Queen"&winner=="white")

wh_queen_lose<-clean_num%>%
  filter(piece=="White Queen"&winner=="black")

# Make plots
############

# Create chessboard
chessboard<-tibble(
  col = rep(c("a","b","c","d","e","f","g","h"),8),
  col_num = rep(seq(1,8,1),8),
  row = c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8),rep(7,8),rep(8,8)),
  value = paste0(col,row)
)

black=c(
  "a1","c1","e1","g1",
  "b2","d2","f2","h2",
  "a3","c3","e3","g3",
  "b4","d4","f4","h4",
  "a5","c5","e5","g5",
  "b6","d6","f6","h6",
  "a7","c7","e7","g7",
  "b8","d8","f8","h8"
)

chessboard<-chessboard%>%
  mutate(clr=case_when(
    value%in%black~"black",
    TRUE~"white"
  ))

pal=c(
  "black"="#7C9AA2",
  "white"="#F8FBEF"
)


baseplot<-ggplot()+
  geom_rect(
    data=chessboard%>%
      filter(clr=="black"),
    mapping=aes(
      xmin=col_num-0.5,xmax=col_num+0.5,
      ymin=row-0.5,ymax=row+0.5
    ),
    fill=pal[1]
  )+
  geom_rect(
    data=chessboard%>%
      filter(clr=="white"),
    mapping=aes(
      xmin=col_num-0.5,xmax=col_num+0.5,
      ymin=row-0.5,ymax=row+0.5
    ),
    fill=pal[2]
  )+
  guides(fill='none')+
  coord_fixed()+
  scale_x_continuous(
    limits=c(0.5,8.5),
    breaks=seq(1,8,1),
    labels=c("a","b","c","d","e","f","g","h")
  )+
  scale_alpha(range=c(0,0.75))+
  scale_y_continuous(limits=c(0.5,8.5),breaks=seq(1,8,1))+
  scale_fill_scico(palette = 'acton')+ 
  guides(alpha='none')+
  labs(x="",y="")+
  theme_minimal()+
  theme(
    panel.grid=element_blank(),
    axis.text=element_text(family="ral",size=25,face="bold")  
  )

baseplot

# Plot results on the chessboard
wh_qu_w<-baseplot+
  stat_density_2d(
    data=wh_queen_win%>%
      filter(cell!="d1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=4,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_qu_w

wh_qu_l<-baseplot+
  stat_density_2d(
    data=wh_queen_lose%>%
      filter(cell!="d1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=4,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_qu_l

wh_pa_w<-baseplot+
  stat_density_2d(
    data=wh_pawn_win%>%
      filter(row!=2),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=100
  )+
  annotate(
    geom="point",
    pch=21,
    x=1,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=2,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=3,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=4,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=5,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=6,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=7,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=8,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_pa_w

wh_pa_l<-baseplot+
  stat_density_2d(
    data=wh_pawn_lose%>%
      filter(row!=2),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=100
  )+
  annotate(
    geom="point",
    pch=21,
    x=1,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=2,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=3,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=4,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=5,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=6,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=7,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=8,y=2,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_pa_l

wh_ro_w<-baseplot+
  stat_density_2d(
    data=wh_rook_win%>%
      filter(cell!="a1"&cell!="h1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=1,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=8,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_ro_w

wh_ro_l<-baseplot+
  stat_density_2d(
    data=wh_rook_lose%>%
      filter(cell!="a1"&cell!="h1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=1,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=8,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_ro_l

wh_kn_w<-baseplot+
  stat_density_2d(
    data=wh_knight_win%>%
      filter(cell!="b1"&cell!="g1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=2,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=7,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_kn_w

wh_kn_l<-baseplot+
  stat_density_2d(
    data=wh_knight_lose%>%
      filter(cell!="b1"&cell!="g1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=2,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=7,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_kn_l


wh_bi_w<-baseplot+
  stat_density_2d(
    data=wh_bishop_win%>%
      filter(cell!="c1"&cell!="f1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=3,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=6,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_bi_w

wh_bi_l<-baseplot+
  stat_density_2d(
    data=wh_bishop_lose%>%
      filter(cell!="c1"&cell!="f1"),
    mapping=aes(
      x=col_jitter,y=row_jitter,
      fill = after_stat(level),
      alpha = (..level..)
    ),
    geom = "polygon",
    bins=500
  )+
  annotate(
    geom="point",
    pch=21,
    x=3,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="point",
    pch=21,
    x=6,y=1,
    fill=scico(3, palette = 'acton')[3],
    color=scico(3, palette = 'acton')[1],
    size=10
  )+
  annotate(
    geom="rect",
    xmin=0.5,xmax=8.5,ymin=0.5,ymax=8.5,
    color="dimgrey",fill=NA
  )

wh_bi_l
