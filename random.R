random.pick <- function(lineup, valid.heroes.id, side) {
  feed.in <- FALSE
  while(feed.in == FALSE) {
    random.hero <- sample(1:128, size = 1)
    if (lineup[random.hero] == 0 & random.hero %in% valid.heroes.id) {
      lineup[random.hero] <- side
      feed.in <- TRUE
    }
  }
  return(lineup)
}