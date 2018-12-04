# Store the states of visited nodes
node.states <- list()
valid.heroes.id <- c()

mcts.pick <- function(lineups, valid.heroes.id, side, all.candidates, max.tryout, random.state, ucb.bias) {
  # Pick first randomly
  if (all(lineups == 0)) {
    pick <- sample.int(length(lineups), 1)
    lineups[pick] = side
    return(lineups)
  }
  
  # Make the best pick and return modified lineups
  current.id <- state.id(lineups)
  pick.candidates <- valid.next.picks(lineups = lineups,
                                      valid.heroes.id = valid.heroes.id,
                                      all.candidates = all.candidates,
                                      max.tryout = max.tryout,
                                      random.state = random.state)
  pick <- evaluate.candidates(candidates = pick.candidates,
                              lineups = lineups,
                              parent.id = current.id,
                              side = side,
                              ucb.bias = ucb.bias)
  lineups[pick] <- side
  return(lineups)
}

state.id <- function(lineups) {
  library(digest)
  return(digest(lineups, "md5"))
}

state.available <- function(node.states, state.id){
  return(!is.null(node.states[[as.character(state.id)]]))
}

valid.next.picks <- function(lineups, valid.heroes.id, all.candidates, max.tryout, random.state) {
  valid.picks <- c()
  for (i in 1:length(lineups)) {
    if (lineups[i] == 0 && is.element(i, valid.heroes.id)) {
      valid.picks <- append(valid.picks, i)
    }
  }
  if (all.candidates) {
    return(valid.picks)
  } else if(max.tryout > length(valid.picks)) {
    print("max.tryout is larger than all available candidates, return all instead")
    return(valid.picks)
  } else {
    set.seed(random.state)
    part <- sample(1:length(valid.picks), max.tryout, replace = FALSE)
    return(valid.picks[part])
  }
}

evaluate.candidates <- function(candidates, lineups, parent.id, side, ucb.bias) {
  # Within all the candidates, only explore the ones without available node state
  lineups.to.explore <- c()
  for (i in 1:length(candidates)){
    cad.lineups <- lineups
    cad.lineups[candidates[i]] <- side
    if (!state.available(state.id(cad.lineups))) {
      lineups.to.explore <- append(lineups.to.explore, cad.lineups)
    }
  }
  
  # Rollout simulations on all lineups to explore
  if (length(lineups.to.explore) != 0) {
    for(i in 1:length(lineups.to.explore)){
      simulations <- list()
      simulations[[i]] <- simulate(start.lineups = lineups.to.explore[i], valid.heroes.id = valid.heroes.id)
      update.node.states(node.states, simulations);
    }
  }
  
  # Updating values based on estimated probs
  props <- c()
  visited <- c()
  for (i in 1:length(candidates)){
    cad.lineups <- lineups
    cad.lineups[candidates[i]] <- side
    cad.state.id <- state.id(cad.lineups)
    w <- node.states[[cad.state.id]][["w"]]
    v <- node.states[[cad.state.id]][["v"]]
    visited[i] <- v
    if (side == 1) {
      props[i] <- w/v
    } else {
      props[i] <- 1-(w/v)
    }
  }
  visits <- node.states[[as.character(parent.id)]][["v"]][1]
  if (is.null(visits)) {
    visits <- 1
  }
  ME <- ucb.bias * sqrt(max(log(visits), 1) / visited)
  UCB <- props + ME
  
  # Making decision
  choice.index.chosen <- UCT(props, UCB, weight = weight)
  pick <- candidates[choice.index.chosen]
  return(pick)
}

simulate <- function(start.lineup, valid.heroes.id) {
  tmp.lineups <- start.lineup
  r.left.picks <- 5 - sum(tmp.lineups > 0)
  d.left.picks <- 5 - sum(tmp.lineups < 0)
  total.left.picks <- r.left.picks + d.left.picks
  picked.ids <- c()
  for (i in 1:length(start.lineup)) {
    if (start.lineup[i] != 0) {
      picked.ids <- append(picked.ids, i)
    }
  }
  left.ids <- valid.heroes.id[! valid.heroes.id %in% picked.ids]
  random.picks <- sample(left.ids, total.left.picks, replace = FALSE)
  print(random.picks)
  for (i in 1:total.left.picks) {
    if (i <= r.left.picks) {
      tmp.lineups[random.picks[i]] <- 1
    } else {
      tmp.lineups[random.picks[i]] <- -1
    }
  }
  return(tmp.lineups)
}
