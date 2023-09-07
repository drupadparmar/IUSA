
# Function of simulation model

# models a queueing network consisting of three consecutive M/M/1 queues, where the final node
# requires maintenance - this means at certain time points the server is out of action (being repaired)
# and only starts serving after being fixed

# currently the intervals for maintenance and the time required to fix are constant

# three outputs are returned
# 1. the time in system for each entity
# 2. the system state upon arrival of each entity
# 3. the parameters used to run the model

###################################################################################################

# lambda = arrival rate
# mu1 = service rate of server 1
# mu2 = service rate of server 2
# mu3 = service rate of server 3
# maintenance_interval = time period between maintenances
# fix_time = time required to fix server
# tend = ending time of the simulation

simulate <- function(lambda, mu1, mu2, mu3, maintenance_interval, fix_time, tend){
  
  # error checking
  pars <- c(lambda, mu1, mu2, mu3, maintenance_interval, fix_time, tend)
  if (sum(pars < 0) > 0){stop("all arguments must be positive")}
  if (maintenance_interval < fix_time){stop("fix_time must be smaller than maintenance_interval")}
  
  # function to model an arrival event
  arrival_fn <- function(){
    
    # generate and store NEXT arrival time
    next_arrival <<- clock + rexp(1, rate = lambda)
    arrival_times <<- c(arrival_times, next_arrival)
    
    # measure state on arrival
    state <<- rbind(state, c(busy, sapply(queue, length), broken_n3))
    
    # if server 1 is not busy then immediate service
    if (busy[1] < 1){
      busy[1] <<- 1
      next_departure[1] <<- clock + rexp(1, rate = mu1)
    }
    
    # if server 1 is busy then join the queue
    else{
      queue[[1]] <<- append(queue[[1]], clock)
    }
  }
  
  # models departure event from node 1
  departure_n1_fn <- function(){
    
    # get queue length
    q_length <- length(queue[[1]])
    
    # if queue exists, front of queue receives service
    if (q_length > 0){ 

      next_departure[1] <<- clock + rexp(1, rate = mu1) # generate depature time
      
      # update queue
      if (q_length == 1){
        queue[1] <<- list(NULL)
      }
      else {
        queue[[1]] <<- queue[[1]][2:q_length]
      }
    }
    
    # if there is no queue, then server becomes free
    else{
      busy[1] <<- 0
    }
    
    # if server 2 not busy then immediate service
    if (busy[2] < 1){
      
      busy[2] <<- 1 # update number of busy servers
      next_departure[2] <<- clock + rexp(1, rate = mu2) # generate departure time
    }
    # if server 2 is busy then join the queue
    else{
      queue[[2]] <<- append(queue[[2]], clock)
    }
    
  }
  
  # models departure event from node 2
  departure_n2_fn <- function(){
    
    # get queue length
    q_length <- length(queue[[2]])
    
    # if queue exists, front of queue receives service
    if (q_length > 0){ 
      
      next_departure[2] <<- clock + rexp(1, rate = mu2) # generate departure time
      
      # update queue
      if (q_length == 1){
        queue[2] <<- list(NULL)
      }
      else {
        queue[[2]] <<- queue[[2]][2:q_length]
      }
    }
    
    # if there is no queue, then server becomes free
    else{
      busy[2] <<- 0
    }  
    
    # if server 3 is not busy or broken, then immediate service
    if (busy[3] < 1 && broken_n3 == FALSE){
      
      busy[3] <<- 1 # update number of busy servers
      next_departure[3] <<- clock + rexp(1, rate = mu3) # generate departure time
    }
    # if server 3 is busy then join the queue
    else{
      queue[[3]] <<- append(queue[[3]], clock)
    }
    
  }
  
  # models departure event from node 3
  departure_n3_fn <- function(){
    
    # store departure time
    departure_times <<- c(departure_times, clock)
    
    # get queue length
    q_length <- length(queue[[3]])
    
    # if machine working
    if (broken_n3 == FALSE){
    
      # if queue exists, front of queue receives service
      if (q_length > 0){
        
        next_departure[3] <<- clock + rexp(1, rate = mu3) # generate departure time
        
        # update queue
        if (q_length == 1){
          queue[3] <<- list(NULL)
        }
        else {
          queue[[3]] <<- queue[[3]][2:q_length]
        }
      }
      
      # if there is no queue, then server becomes free
      else{
        busy[3] <<- 0
      }
    }
    
    # if machine is broken 
    else{
      busy[3] <<- 0 # update number of bsuy servers
      next_departure[3] <<- Inf  # no scheduled departures
    }
    
  }
  
  # models a maintenance event
  maintenance_fn <- function(){
    
    # set fixture time
    if (next_departure[3] == Inf){
      # if no-one in service, fix straight away
      next_fix <<- next_maintenance + fix_time
    }
    else{ # else wait until departure then fix
      next_fix <<- next_departure[3] + fix_time
    }
    
    next_maintenance <<- next_maintenance + maintenance_interval # set next maintenance time
    broken_n3 <<- TRUE # update broken boolean
  }
  
  # models a fix event
  fix_fn <- function(){
    
    broken_n3 <<- FALSE # update broken boolean
    next_fix <<- next_maintenance + fix_time # set fixture time
    
    q_length <- length(queue[[3]]) # get queue length
    
    # if there is a queue, front of queue receives service
    if (q_length > 0){
      
      busy[3] <<- 1 # update busy server
      next_departure[3] <<- clock + rexp(1, rate = mu3) # generate departure time
      
      # if 1 in queue
      if (q_length == 1){
        queue[3] <<- list(NULL) # no queue exists
      }
      # if > 1 in queue
      else {
        queue[[3]] <<- queue[[3]][2:q_length] # remove first element
      }
    }
    
    # if no queue, then no departure time
    else{
      next_departure[3] <<- Inf # update departure time
    }
    
  }
  
  
  # function to find next event, update clock, and return event type
  timer_q <- function(){
    
    # find next event
    event_index <- which.min(c(next_arrival, next_end_sim, next_departure[1], next_departure[2],
                              next_departure[3], next_maintenance, next_fix))
    # update clock and temporary next event time
    switch(event_index,
           
           {timer_event <- "Arrival"
           clock <<- next_arrival
           next_arrival <<- Inf}, # 1 = Arrival
           
           {timer_event <- "EndSimulation"
           clock <<- next_end_sim
           next_end_sim <<- Inf},  # 2 = EndSimulation
           
           {timer_event <<- "Departure_n1"
           clock <<- next_departure[1]
           next_departure[1] <<- Inf}, # 3 = Departure from node 1
           
           {timer_event <<- "Departure_n2"
           clock <<- next_departure[2]
           next_departure[2] <<- Inf}, # 4 = Departure from node 2
           
           {timer_event <<- "Departure_n3"
           clock <<- next_departure[3]
           next_departure[3] <<- Inf}, # 5 = Departure from node 3
           
           {timer_event <<- "Maintenance"
           clock <<- next_maintenance}, # 6 = Maintenance
           
           {timer_event <<- "Fix"
           clock <<- next_fix} # 7 = Fix
    )
    # return event type
    return(timer_event)
  }
  
  # main simulation loop; execution starts here
  queue <<- vector("list", 3) # lists to store queues at each node
  busy <<- rep(0, 3) # binary vector indicating which servers are busy  
  broken_n3 <<- FALSE # boolean of broken machine
  clock <<- 0
  next_arrival <<- rexp(1, rate = lambda)
  next_departure <- rep(Inf, 3) # vector of next departure times from each node
  next_maintenance <<- maintenance_interval # next maintenance
  next_fix <<- next_maintenance + fix_time # next fix
  next_end_sim <<- tend
  next_event <- ""
  
  # store arrival and (final) departure times
  arrival_times <- c(next_arrival)
  departure_times <- NULL
  
  # store state - busy servers and number in queue for each node
  state <- NULL
  
  # simulation loop
  while(next_event != "EndSimulation"){
    next_event <- timer_q()
    switch(next_event,
           Arrival = arrival_fn(),
           Departure_n1 = departure_n1_fn(),
           Departure_n2 = departure_n2_fn(),
           Departure_n3 = departure_n3_fn(),
           Maintenance = maintenance_fn(),
           Fix = fix_fn()
    )
  }
  
  # label states
  colnames(state) <- c(paste0("B", 1:3), paste0("Q", 1:3), "M3")
  
  # find number that leave system
  n <- length(departure_times) 
  
  # subset outputs accordingly
  tis <- departure_times - arrival_times[1:n]
  state <- state[1:n, ]
  
  # return list of outputs
  return(list("time_in_system" = tis,
              "state" = state,
              "parameters" = pars))
  
}
