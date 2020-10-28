manhattan_distance = function (loc, destination)
{
  x_distance = abs(loc[[1]] - destination[[1]])
  y_distance = abs(loc[[2]] - destination[[2]])
  return(x_distance+y_distance)
}
explore_node = function (x, y, strdirection, path, destination, frontier_dis, frontier_his, cost)
  
#explore_node = function (x, y, strdirection, path, destination, frontier_dis, frontier_his, cost, explored_dis, explored_his)
{
  path = paste(path, strdirection ,sep=",")
  
  direction  <- matrix(data=Inf, nrow = 1, ncol = 4)
#  colnames(direction) = c("X", "Y", "cost", "heuristic")
  direction[1, ] = c(x, y, cost, manhattan_distance(list(x, y), destination))
  direction_path = matrix(data=NA, nrow =1, ncol = 1)
  colnames(direction_path) = "path"
  direction_path[1, ] = path
  
  if (any(frontier_dis[, "X"]==direction[, "X"] & frontier_dis[, "Y"]==direction[, "Y"])) {
    other_route_dis = subset(frontier_dis, x==direction[, "X"] & y==direction[, "Y"])
   if(other_route_dis[, "cost"] + other_route_dis[, "heuristic"] > direction[, "cost"] + direction[, "heuristic"]) {
      frontier_dis[frontier_dis[, "X"]==direction[, "X"] & frontier_dis[, "Y"]==direction[, "Y"],] = direction
      frontier_his[frontier_dis[, "X"]==direction[, "X"] & frontier_dis[, "Y"]==direction[, "Y"],] = direction_path
      
    } 
  }
  else {

    frontier_dis = rbind(frontier_dis, direction)
    frontier_his = rbind(frontier_his, direction_path)
  }

  frontier = list(frontier_dis, frontier_his)
  return(frontier)
}

compute_travel_cost = function (x, y, destination, roads)
{
  
  frontier_dis  <- matrix(data=Inf, nrow = 1, ncol = 4)
  colnames(frontier_dis) = c("X", "Y", "cost", "heuristic")
  frontier_dis[1, ] = c(x, y, 0, manhattan_distance(list(x, y), destination))

  frontier_his = matrix(data=NA, nrow =1, ncol = 1)
  colnames(frontier_his) = "path"
  frontier_his[1, ] = "none"
  
  hroads = roads$hroads
  vroads = roads$vroads
  expansion_row = which.min(frontier_dis[, "cost"] + frontier_dis[, "heuristic"])
  expansion_dis = frontier_dis[expansion_row, , drop=FALSE]
  expansion_his = frontier_his[expansion_row, , drop=FALSE]
  
  #add explored list
  explored_dis = frontier_dis[expansion_row,, drop=FALSE]
#  explored_his = frontier_his[expansion_row,, drop=FALSE]
  
    while (!identical(as.numeric(list(expansion_dis[1, "X"], expansion_dis[1, "Y"])), as.numeric(destination))) {

    frontier_dis = frontier_dis[-c(expansion_row), , drop=FALSE]
    frontier_his = frontier_his[-c(expansion_row), , drop=FALSE]

    x = expansion_dis[, "X"]
    y = expansion_dis[, "Y"]
    orgin_path = expansion_his[, "path"]
    
    #total_cost = expansion$cost + expansion$heuristic + 1  #it is not used!!?!?!?

    trace_explored <- matrix(Inf, 11, 11)
    for (i in length(explored_dis[,1]))
    {trace_explored[c(explored_dis[, 1:2, drop=FALSE][i, ])]=1}
    
    if (x + 1 <= 10 ){if ( is.infinite(trace_explored[x+1, y])){ #new condition
      dircost = hroads[x,y]+expansion_dis[, "cost"]
      frontier = explore_node(x+1, y, 'right', orgin_path, destination, frontier_dis, frontier_his, dircost)
      frontier_dis = frontier[[1]]
      frontier_his = frontier[[2]]
    }}
    if (x - 1 >= 1){if ( is.infinite(trace_explored[x-1, y])){#new condition
      
      dircost = expansion_dis[, "cost"] + hroads[x-1,y]
      frontier = explore_node(x-1, y, 'left', orgin_path, destination, frontier_dis, frontier_his, dircost)
      frontier_dis = frontier[[1]]
      frontier_his = frontier[[2]]

    }}
        if (y + 1 <= 10){if (is.infinite(trace_explored[x, y+1])){#new condition
    
      dircost = expansion_dis[, "cost"] + vroads[x,y]
      frontier = explore_node(x, y+1, 'up', orgin_path, destination, frontier_dis, frontier_his, dircost)
      frontier_dis = frontier[[1]]
      frontier_his = frontier[[2]]

        }}
    if (y - 1 >= 1){if (is.infinite(trace_explored[x, y-1])){#new condition
      
      dircost = expansion_dis[, "cost"] + vroads[x,y-1]
      frontier = explore_node(x, y-1, 'down', orgin_path, destination, frontier_dis, frontier_his, dircost)
      frontier_dis = frontier[[1]]
      frontier_his = frontier[[2]]

        }}
    
    expansion_row = which.min(frontier_dis[, "cost"] + frontier_dis[, "heuristic"])
    expansion_dis = frontier_dis[expansion_row, , drop=FALSE]
    expansion_his = frontier_his[expansion_row, , drop=FALSE]    
    
    #add explored list
    explored_dis = rbind(explored_dis, frontier_dis[expansion_row, , drop=FALSE]) 
#    explored_his = rbind(explored_his, frontier_his[expansion_row, , drop=FALSE]) 


  }
  
  final_path = as.list(strsplit(expansion_his[, "path"], ",")[[1]])
  if (length(final_path) == 1) {
    return(5)
  }
  else if (final_path[[2]] == 'up') {
    return(8)
  }
  else if (final_path[[2]] == 'down') {
    return(2)
  }
  else if (final_path[[2]] == 'left') {
    return(4)
  }
  else if (final_path[[2]] == 'right') {
    return(6)
  }
}

pack_dists = function (order_pickup,drop2load,car2load){
  dist = car2load[[order_pickup[1]]]
  for (i in 2:5){
    dist = dist + drop2load[order_pickup[i-1], order_pickup[i]]
  }
  return(dist)
}

get_car_path = function(carpos, packageposes) {
  fromdrop2load = matrix(nrow=5,ncol=5)
  car2load = list()
  for (i in 1:5){car2load[i] = manhattan_distance(carpos, c(packageposes[[i,1]] , packageposes[[i,2]]))}
  for (i in 1:5){
    for (j in 1:5){
      fromdrop2load[[i,j]] = manhattan_distance(c(packageposes[[i,3]] , packageposes[[i,4]]), c(packageposes[[j,1]] , packageposes[[j,2]]))
    }
  }
  perms = matrix(nrow=120,ncol=5)
  current_perm = c()
  r=1
  for (i in 1:5) {
    current_perm[1] = i
    for (j in 1:5) { 
      if (j != i) { 
        current_perm[2] = j
        for (k in 1:5) { 
          if (k != i & k != j) { 
            current_perm[3] = k
            for (l in 1:5) { 
              if (l != i & l != j & l != k) { 
                current_perm[4] = l
                m = 15 - (i+j+k+l)
                current_perm[5] = m
                perms[r,] = current_perm
                r=r+1
              }
              else {next}
            }
          }
          else {next}
        }
      }
    }
  }
  minim = 1000
  for (p in 1:120){
    dist = pack_dists(perms[p,],fromdrop2load,car2load)
    if (dist < minim){
      minim = dist
      order_pack = perms[p,]
    }
  }
  return(order_pack)
}



BB = function (roads, car, packages)
{
  nextMove = 0
  toGo = 0
  offset = 0
  package_data = as.data.frame(packages)
  if (length(car$mem) == 0) {car$mem = c(get_car_path(c(car$x,car$y),package_data), 1, 0,0)}
  if (package_data[car$mem[[car$mem[[6]]]],5] == 2) {car$mem[[6]] = car$mem[[6]] + 1}
  
  if (car$load == 0) {
    toGo = car$mem[[car$mem[[6]]]]
    destination = list(package_data[toGo, 1 + offset], package_data[toGo, 2 + offset])
  }
  else {
    toGo = car$load
    offset = 2
    destination = list(package_data[toGo, 1 + offset], package_data[toGo, 2 + offset])
  }
  car$nextMove = compute_travel_cost(car$x, car$y, destination, roads)
  return(car)
}
