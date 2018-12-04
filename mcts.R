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

  # Simulations on all lineups to explore
  if (length(lineups.to.explore) != 0) {
    for(i in 1:length(lineups.to.explore)){
      sim.result <- simulate(start.lineups = lineups.to.explore[i], valid.heroes.id = valid.heroes.id)
      update.node.states(node.states, sim.result);
    }
  }

  # Updating values based on estimated probs
  props <- c()
  visited <- c()
  for (i in 1:length(candidates)) {
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

  # Selection
  ucb.index <- UCT(props, UCB, weight = TRUE)
  pick <- candidates[ucb.index]
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
  # evaluation of lineup
  radiant.win <- predict.win(tmp.lineups)
  return(list(lineups = tmp.lineups, radiant.win = radiant.win))
}

UCT <- function(props, UCB, weight=TRUE){
  n <- length(props)
  indices <- 1:n
  keep <- c()
  drop <- c()
  for (i in 1:n){
    if (sum(props[i] <= UCB[-i]) == 0)
      keep <- i
    if (sum(UCB[i] >= props[-i]) == 0)
      drop <- i
  }
  if (!is.null(keep)) {
    choice <- keep
  } else {
    if(!is.null(drop)){
      indices <- indices[-drop]
    }
    if (length(indices) > 1){
      if (weight) {
        if (sum(props[indices]==0) == length(indices)) {
          # All choices lose, random pick one
          choice <- sample(indices, 1)
        } else {
          # Select one of the highest win rate ones (not the highest one)
          choice <- sample(indices, 1, prob=props[indices])
        }
      } else {
        choice <- sample(indices, 1)
      }
    } else {
      choice <- indices
    }
  }
  return(choice)
}

update.node.states <- function(sim) {
    leaf <- sim$last.state
    vs <- sim$visited.states
    for (ii in vs){
      cur.str <- as.character(ii)
      if (is.null(branch.leaf.relationships[[cur.str]])){
        branch.leaf.relationships[[cur.str]][["l"]] <<- rep(leaf,2)
        branch.leaf.relationships[[cur.str]][["l"]] <<- leaf
      } else{
        all.leaves <- branch.leaf.relationships[[cur.str]][["l"]]
        all.leaves <- unique(c(all.leaves,leaf))
        branch.leaf.relationships[[cur.str]][["l"]] <<- all.leaves
      }

      if (ii == leaf){
        branch.leaf.relationships[[cur.str]][["w"]] <<- sim$winning.player
      }
      if (ii != leaf) {
        # Update visits count
        if (is.null(branch.leaf.relationships[[cur.str]][["v"]])){
          branch.leaf.relationships[[cur.str]][["v"]] <<- 1
        } else {
          k <- branch.leaf.relationships[[cur.str]][["v"]]
          branch.leaf.relationships[[cur.str]][["v"]] <<- k+1
        }

        # Update wins count
        r <- branch.leaf.relationships[[cur.str]][["w"]]
        if (is.null(r)) r <- 0
        if (sim$winning.player==1){
          branch.leaf.relationships[[cur.str]][["w"]] <<- r + 1
        } else {
          branch.leaf.relationships[[cur.str]][["w"]] <<- r + 0
        }
      }
    }
}

predict.win <- function(lineups) {

}
