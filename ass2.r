myFunction = function(moveInfo, readings, positions, edges, probs) {
  # we initialize transmission matrix
  if (moveInfo$mem$status == 0 | moveInfo$mem$status == 1) {
    trans_matrix = matrix(0, nrow = 40, ncol = 40)
    for (waterhole in 1:40){
      attac_nodes = getOptions(waterhole, edges)
      trans_prob = 1/(length(attac_nodes))
    for (node in attac_nodes){
      trans_matrix[waterhole, node] = trans_prob}}
    moveInfo$mem$trans_m = trans_matrix}

  
  # update the emission probabilities every turn
  emission_probs = numeric(40)
  sali_matrix = probs[[1]]
  phos_matrix = probs[[2]]
  nitro_matrix = probs[[3]]
  for (waterhole in 1:40){
    emission_prob = dnorm(readings[1], mean = sali_matrix[waterhole, 1], sd = sali_matrix[waterhole,2]) * dnorm(readings[2], mean = phos_matrix[waterhole,1], sd = phos_matrix[waterhole,2])* dnorm(readings[3], mean = nitro_matrix[waterhole,1], sd = nitro_matrix[waterhole,2])
    emission_probs[waterhole] = emission_prob}
  emission_probs = emission_probs/sum(emission_probs)
  
  # Initialize the previous node states
  if (moveInfo$mem$status == 0 | moveInfo$mem$status == 1){
    node_state = numeric(40)
    if(!is.na(positions[1]) && positions[1] < 0){
      node_state[positions[1]*-1] = 1}
      if(!is.na(positions[2]) && positions[2] < 0){
        node_state[positions[2]*-1] = 1}
      node_state[positions[3]] = 0
      node_state = node_state + (1/length(which(node_state == 0)))
      moveInfo$mem$nodes_state = node_state}
  
  # update the states in different nodes 
  new_prob = numeric(40)
  
  nodes_state = moveInfo$mem$nodes_state
  trans_m = moveInfo$mem$trans_m
  
  if(!is.na(positions[1]) && positions[1] < 0){
    new_prob[positions[1]*-1] = 1}
  if(!is.na(positions[2]) && positions[2] < 0){
    new_prob[positions[2]*-1] = 1}

  else{
    for (i in 1:40){
      sum_node = numeric(40)
      for (j in 1:40){
        sum_node[j] = nodes_state[j]*trans_m[j, i]}
      new_prob[i] = sum(sum_node)*emission_probs[i]}
    if(!is.na(positions[1]) && positions[1] > 0){
      new_prob[positions[1]]=0}
    if(!is.na(positions[2]) && positions[2] > 0){
      new_prob[positions[2]]=0}
    new_prob[positions[3]]=0
    new_prob = new_prob/sum(new_prob)}
    croc_loca = which.max(new_prob)
    if (croc_loca == positions[3]){
      moveInfo$moves=c(0,0)}
  attac_nodes = getOptions(positions[3], edges)

  if (croc_loca %in% attac_nodes){
    moveInfo$moves = c(croc_loca, 0)}

  else{
    past = c(positions[3])
    queue = c(positions[3])
    parents = numeric(40)
    parents[positions[3]] =0
      
    while (length(queue) != 0){
      current = queue[1]
      queue = queue[-1*which(queue ==current)]
      neighbors = getOptions(current, edges)
      neighbors = neighbors[-1*which(neighbors ==current)]
      neighbors = setdiff(neighbors, past)
      for (node in neighbors){
        if (!(node %in% past)) {
          queue = c(queue, node)
          parents[node] = current
          past = c(past, node)}}}
      current = croc_loca
      path = numeric()
    while (current != 0){
      if (parents[current] != 0){
        path = c(c(current), path)}
        current = parents[current]}
    moveInfo$moves = c(path[1], path[2])}
  moveInfo$mem$nodes_state = new_prob
  moveInfo$mem$status = 2
  
  return(moveInfo)}

getOptions = function (point, edges) {
  c(edges[which(edges[, 1] == point), 2], edges[which(edges[,2] == point), 1], point)}