# Store the states of visited nodes
node.states <- list() # Node States Store
vhi <- 1:20 # Valid Heroes Id List

mcts.pick <- function(lineups, side, valid.heroes.id, n.sims, ucb.bias) {
  # Pick first randomly
  if (all(lineups == 0)) {
    pick <- sample.int(length(lineups), 1)
    lineups[pick] = side
    return(lineups)
  }
  can.heroes.id <- valid.next.picks(lineups = lineups,
                                    valid.heroes.id = valid.heroes.id)
  pick <- eval.candidates(candidates = can.heroes.id,
                          current.lineups = lineups,
                          side = side,
                          n.sims = n.sims,
                          ucb.bias = ucb.bias)
  print(length(node.states))
  print(pick)
  lineups[pick] <- side
  return(lineups)
}

valid.next.picks <- function(lineups, valid.heroes.id) {
  valid.picks <- c()
  for (i in 1:length(lineups)) {
    if (lineups[i] == 0 && is.element(i, valid.heroes.id)) {
      valid.picks <- append(valid.picks, i)
    }
  }
  return(valid.picks)
}

eval.candidates <- function(candidates, current.lineups, side, n.sims, ucb.bias) {
  # Within all the candidates, only explore the ones without available node state
  lineups.to.explore <- list()
  lte.idx <- 1
  for (i in 1:length(candidates)){
    cad.lineups <- current.lineups
    cad.lineups[candidates[i]] <- side
    cad.lineups.id <- state.id(cad.lineups)
    if (is.null(node.states[[cad.lineups.id]])) {
      lineups.to.explore[[lte.idx]] = cad.lineups
      lte.idx <- lte.idx + 1
    }
  }
  
  # Simulations on all lineups to explore
  if (length(lineups.to.explore) != 0) {
    for(i in 1:length(lineups.to.explore)){
      # Perform n.sims times of simulations, and update the node states accordingly
      for (j in 1:n.sims) {
        ## 3. Simulation
        sim.result <- simulate(start.lineups = lineups.to.explore[[i]], avail.heroes.id = candidates)
        ## 4. Backpropagation
        update.node.states(sim = sim.result);
      }
    }
  }
  # Updating values based on estimated probs
  props <- c()
  visited <- c()
  for (i in 1:length(candidates)) {
    cad.lineups <- current.lineups
    cad.lineups[candidates[i]] <- side
    cad.lineups.id <- state.id(cad.lineups)
    wr <- node.states[[cad.lineups.id]][["wr"]]
    v <- node.states[[cad.lineups.id]][["v"]]
    visited[i] <- v
    if (side == 1) {
      props[i] <- wr/v
    } else {
      props[i] <- 1 - (wr/v)
    }
  }
  # Calculate backpropagation scores
  cl.id <- state.id(current.lineups)
  cl.visited <- node.states[[cl.id]][["v"]]
  if (is.null(cl.visited)) {
    cl.visited <- 1
  }
  ucb <- props + ucb.bias * sqrt(max(log(cl.visited), 1) / visited)
  print(props)
  print(ucb)
  
  # Making decision
  ucb.index <- uct(props, ucb, weight = TRUE)
  pick <- candidates[ucb.index]
  return(pick)
}

uct <- function(props, ucb, weight=TRUE) {
  n <- length(props)
  indices <- 1:n
  keep <- c()
  drop <- c()
  for (i in 1:n){
    if (sum(props[i] <= ucb[-i]) == 0)
      keep <- i
    if (sum(ucb[i] >= props[-i]) == 0)
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
  wr <- sim$radiant.win.rate
  vss <- sim$visited.states
  for (vs in vss) {
    vs.id <- state.id(vs)
    if (is.null(node.states[[vs.id]])){
      node.states[[vs.id]][["wr"]] <<- 0
      node.states[[vs.id]][["v"]] <<- 0
    }
    visited <- node.states[[vs.id]][["v"]]
    node.states[[vs.id]][["v"]] <<- visited + 1
    win.rate <- node.states[[vs.id]][["wr"]]
    node.states[[vs.id]][["wr"]] <<- win.rate + wr
  }
}

simulate <- function(start.lineups, avail.heroes.id) {
  tmp.lineups <- start.lineups
  r.left.picks <- 5 - sum(tmp.lineups > 0)
  d.left.picks <- 5 - sum(tmp.lineups < 0)
  total.left.picks <- r.left.picks + d.left.picks
  picked.ids <- c()
  for (i in 1:length(start.lineups)) {
    if (start.lineups[i] != 0) {
      picked.ids <- append(picked.ids, i)
    }
  }
  left.ids <- avail.heroes.id[! avail.heroes.id %in% picked.ids]
  random.picks <- sample(left.ids, total.left.picks, replace = FALSE)
  visited.states <- list()
  visited.states[[1]] <- start.lineups
  for (i in 2:total.left.picks+1) {
    if (i <= r.left.picks) {
      tmp.lineups[random.picks[i]] <- 1
    } else {
      tmp.lineups[random.picks[i]] <- -1
    }
    visited.states[[i]] <- tmp.lineups
  }
  # evaluation of lineup
  r.win.rate <- predict.win.rate(tmp.lineups)
  return(list(radiant.win.rate = r.win.rate, visited.states = visited.states))
}

predict.win.rate <- function(lineups) {
  ########
  return(runif(1, min = 0.2, max = 0.8))
  ########
}

state.id <- function(lineups) {
  library(digest)
  return(digest(lineups, "md5"))
}
