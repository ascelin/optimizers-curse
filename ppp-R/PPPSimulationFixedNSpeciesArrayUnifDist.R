rm(list=ls())


# Translation of code from "PPP_Simulation_fixed_nspecies_arrau_unif_dist.m MATLAB 
# code written by Chris Baker

#option to load the matlab package for R which duplicates some functions
#install.packages("matlab")
#library(matlab)

#load functions to mimic MATLAB functions for flipping matrices horizontal or vertical
fliplr <- function (x) x[, ncol(x):1]
flipud <- function (x) x[nrow(x):1, ]
# mimic the repmat function
repmat <- function (mat, rows, cols)
  kronecker(matrix(1, rows, cols), mat)

# pair of functions to mimic is.member
row_list <- function (x) {
  x <- as.matrix(x)
  lis <- apply(x, 1, list)
  lapply(lis, `[[`, 1)
}

row_member <- function(a, b){
  row_list(a) %in% row_list(b)
}

is_member <- function(a, b){
  as.vector(a) %in% as.vector(b)
}


#load random numbers from the separately generated file
usefile <- TRUE

if (usefile == TRUE) { 
  r_nums <- read.csv("random_numbers1000000.csv", header = FALSE)
  r_nums <- r_nums[, 1] 
}

# mimic of function unifrnd_2 from Matlab code
# uses random number file to resturn a matrix of uniform random variables betwen
# a and b, with dimensions nrow and ncol.
unifrnd_2 <- function(a, b, nrow = 1, ncol = 1) {
  
  num <- nrow * ncol
  
  if (usefile) {
    
    if ((counter + num) > 1e7) {
      counter <<- 1
    }
    
    nums <- r_nums[counter:(counter + num - 1)]
    counter <<- counter + num
    nums <- nums * (b - a) + a
    
  } else {
    
    nums <- runif(num, a, b)
    
  }
  
  output <- matrix(nums, nrow = nrow, ncol = ncol) 
  return (output)
  
}


# mimic of randsample_2 from Matlab file
# function that works on variable opts, using the random number file. 
randsample_2 <- function (opts) {
  
  if (usefile) {
    
    len <- length(opts)
    rnum <- ceiling(r_nums[counter] * len)
    counter <<- counter + 1
    out <- opts[rnum]
    
  } else {
    out <- sample(opts, 1)
  }
  
  return (out)
  
}


#set number of islands and species
# n_islands <- 3
# n_species <- 4
# total_species <- n_islands*n_species
# 
# # set project budget
# budget <- 2e7
# 
# # number of replicates/trials
# reps <- 1000
# cost_uncertainty = TRUE
# # choose random erad to fail if over budget
# # Only has effect when overbudget_fail = false.
# # If false then it will reduce the total benefit of projects proportional to the amount you're over budget. 
# # Note code checks for over_budget_fail first,
# # only checks for this if over_budget_fail = false;
# over_budget_fail = TRUE
# no_value_reduction = TRUE
# interaction_flag = FALSE
# # note see disc_param needs to be set to something other than [1,1] for this to work.
# # set failure probability. 
# # if false, then prob failure set to 0 and B:C and PPP are identical. 
# # If true then have prob of failure for each project.
# failure_prob = TRUE
# # set probability uncertainty
# # This only has an effect when failure_prob = true; 
# # This determineS whether there is uncertainty in the prob of failure
# prob_uncertainty = TRUE



simulation <- function (n_islands = 3,
                        n_species = 4,
                        total_species = n_islands*n_species,
                        budget = 2e7,
                        reps = 1000,
                        # cost uncertainty
                        cost_uncertainty = TRUE,
                        # choose random erad to fail if over budget
                        # Only has effect when overbudget_fail = false.
                        # If false then it will reduce the total benefit of projects proportional to the amount you're over budget.
                        # Note code checks for over_budget_fail first,
                        # only checks for this if over_budget_fail = false;
                        over_budget_fail = TRUE,
                        no_value_reduction = TRUE,
                        interaction_flag = FALSE,
                        # note see disc_param needs to be set to something other than [1,1] for this to work.
                        # set failure probability.
                        # if false, then prob failure set to 0 and B:C and PPP are identical.
                        # If true then have prob of failure for each project.
                        failure_prob = TRUE,
                        # set probability uncertainty
                        # This only has an effect when failure_prob = true;
                        # This determineS whether there is uncertainty in the prob of failure
                        prob_uncertainty = TRUE) {


  # assign counter
  counter <<- 1
  
  # range for discount for multiple spp on a single island
  disc_param <- c(0.7, 0.9)
  # e.g. to make a 20% discount would be disc_param = [0.8,0.8];
  # and disc_param = [0.7,0.8]; discount randomly would range from between 70% and 80%
  
  
  ### parameter generation ###
  
  # true cost - lognormal distribution
  cost_mu <- 1.25
  cost_sigma <- 0.5
  
  # true probability of success - beta distribution
  # smaller = more variation. ~5 for lots of variation, ~30-50 for minimal variation
  prob_shape <- 20
  # average probability of success
  prob_dist_mean <- 0.75
  
  # estimated cost & value - normal distribution
  est_mu <- 1
  est_sigma <- 0.5 # bigger -> more variation
  
  # estimated probability of success & discount amount - beta distribution
  #%smaller = more variation. ~5 for lots of variation, ~30-50 for minimal variation
  shape_prob = 4
  
  
  # Arrays to store the decisions for each of the different methods
  # optimal choice based on the revealed values (after projects succeed/fail)
  rev_opt_array <- matrix(0, reps, 10)
  
  # optimal choice based on the true values
  true_opt_array <- rev_opt_array
  
  # choice using the expected values
  exp_opt_array <- rev_opt_array
  # choice using benefit/cost
  cost_ben_array <- exp_opt_array
  # choice using PPP
  ppp_array <- exp_opt_array
  # random
  rand_sel_array <- exp_opt_array
  
  # create a matrix whose columns are every possible combination of 2^total_species 1's and 0's. 
  # These represent every possible decision
  logical_matrix <- matrix(0, 2 ^ total_species, total_species)
  
  
  for (i in 1:total_species) {
    logical_matrix[, i] <- rep(1:0,
                               each = 2 ^ (i - 1),
                               length.out = 2 ^ total_species)
  }
  
  
  #flip the matrix left-right
  logical_matrix <- fliplr(logical_matrix)
  
  
  # initialise an array that records which projects get cost discounts, given a decision
  disc_array <- matrix(0, 2 ^ total_species, total_species)
  
  for (i in 1:nrow(disc_array)){
    
    for (j in seq(1, n_islands*n_species, n_species)){
      
      # if two or more projects on that island have been chosen, then all projects are given the discount
      disc_array[i,j:(j+n_species-1)] <- sum(logical_matrix[i,j:(j+n_species-1)]) >1
      
    }
    
  }
  
  
  
  
  # large while loop for similating costs and values for each of the matrices
  
  r <- 0
  
  while(r < reps){
    r <- r+1
    r/reps
    
    # generate true values
    # true cost between 3 and 6 million
    true_cost <- unifrnd_2(3, 6, n_islands, n_species)*1e6
    # discount parameter from uniform distribution
    true_disc <- unifrnd_2(disc_param[[1]], disc_param[[2]], n_islands, n_species)
    # true value from between 0 and 1
    true_value <- unifrnd_2(1, 2, n_islands, n_species)
    
    
    if (failure_prob == FALSE){
      # projects always succeed
      true_prob <- matrix(1, n_islands, n_species)
      
    } else {
      # generate true probability of success from .2 to .8.
      true_prob <- unifrnd_2(0.2, 0.8, n_islands, n_species)
    }
    
    
    # draw whether each project is successfull or not
    proj_succ <- true_prob > unifrnd_2(0, 1, nrow(true_prob), ncol(true_prob))
    
    
    
    # check if there is cost uncertainty or not
    if (cost_uncertainty == FALSE){
      # if not, the estimated cost is the true cost
      removal_cost_est <- true_cost
    } else {
      # estimate the cost from +- 1.5 million around the true value
      removal_cost_est <- true_cost + (unifrnd_2(-1.5, 1.5, n_islands, n_species))*1e6
    }
    
    # estimate the value from +-1 around the true value (hence estimated values can be between 0 and 3)
    removal_value_est <- true_value + unifrnd_2(-1, 1, n_islands, n_species)
    
    
    # check if there are interactions
    if (interaction_flag == TRUE){
      # set the maximum range of uncertainty such that discount_est will never be < 0 or > 1.
      unc_range <- min(disc_param[[1]], 1-disc_param[[2]])
      # draw removal discount est from a unifrom distribution
      removal_discount_est <- true_disc + unifrnd_2(-unc_range, unc_range, n_islands, n_species)    
    } else {
      # if no interactions, set removal discount estimate to be the true discount.
      removal_discount_est <- true_disc    
    }
    
    
    # this is the probability of success uncertainty.
    if (prob_uncertainty == TRUE){
      # draw from a uniform distribution
      removal_prob_est <- true_prob + unifrnd_2(-0.2, 0.2, n_islands, n_species)
      
    } else {
      # if no uncertainty, set equal to true value.
      removal_prob_est <- true_prob
    }
    
    
    # calculate the true cost of every project, including the discount
    #turn true cost and true discount into vectors
    true_cost_vec <- as.vector(true_cost)
    true_disc_vec <- as.vector(true_disc)
    # make two cost matrices representing the cost and discounts
    cost_matrix_a <- repmat(t(true_cost_vec), nrow(logical_matrix), 1)
    cost_matrix_b <- repmat(t(true_disc_vec), nrow(logical_matrix), 1)
    # make a new disc_array (discount array)
    # disc_array_2 <- disc_array + !disc_array
    #calculate cost
    cost_matrix <- logical_matrix * cost_matrix_a * (cost_matrix_b * disc_array + !disc_array)
    
    
    # calculate the estimated cost of every project, including the discount
    #turn removal cost estimate and removal discount estimate into vectors
    removal_cost_est_vec <- as.vector(removal_cost_est)
    removal_discount_est_vec <- as.vector(removal_discount_est)
    # make two cost matrices representing the cost and discounts
    est_cost_matrix_a <- repmat(t(removal_cost_est_vec), nrow(logical_matrix), 1)
    est_cost_matrix_b <- repmat(t(removal_discount_est_vec), nrow(logical_matrix), 1)
    # calculate estimated cost
    est_cost_matrix <- logical_matrix * est_cost_matrix_a * (est_cost_matrix_b * disc_array + !disc_array)
    
    
    # calculate the true cost of every set of projects
    # calculate the sum of every row in the cost matrix
    cost_vec <- rowSums(cost_matrix)
    
    # calculate the estimated cost of every set of projects
    est_cost_vec <- rowSums(est_cost_matrix)
    
    # create a vector that stores the true value for each option, weighted by probability of success
    #turn true value and true probability into vectors
    true_value_vec <- as.vector(true_value)
    true_prob_vec <- as.vector(true_prob)
    # make two value matrices representing the value and probability
    value_vec_a <- repmat(t(true_value_vec), nrow(logical_matrix), 1)
    value_vec_b <- repmat(t(true_prob_vec), nrow(logical_matrix), 1)
    #calculate the true value vector
    value_vec <- rowSums(logical_matrix * value_vec_a * value_vec_b)
    
    
    #create a vector that stores the estimated value for each option, weighted by probability of success
    #turn removal value estimates and removal probability estimates into vectors
    removal_value_est_vec <- as.vector(removal_value_est)
    removal_prob_est_vec <- as.vector(removal_prob_est)
    # make two removal value matrices representing the probaility and value
    est_value_vec_a <- repmat(t(removal_value_est_vec), nrow(logical_matrix), 1)
    est_value_vec_b <- repmat(t(removal_prob_est_vec), nrow(logical_matrix), 1)
    # calculate estimated value vector
    est_value_vec <- rowSums(logical_matrix * est_value_vec_a * est_value_vec_b)
    
    
    # create a vector that stores the true value for each option, assuming all successfull
    #value_vec_success = sum(logical_matrix.*repmat(true_value(:)',size(logical_matrix,1),1),2)
    #calculate value of success
    value_vec_success <- rowSums(logical_matrix * value_vec_a)
    
    
    
    # create a vector that stores the estimated value for each option, assuming all successfull
    #est_value_vec_success = sum(logical_matrix.*repmat(removal_value_est(:)',size(logical_matrix,1),1),2)
    #calculate estimated value of success
    est_value_vec_success <- rowSums(logical_matrix * est_value_vec_a)
    
    
    # create a vector that stores the revealed value for each option
    #turn project success into a vector
    proj_succ_vec <- as.vector(proj_succ)
    # make a removal value matrices representing  and value
    project_success <- repmat(t(proj_succ_vec), nrow(logical_matrix), 1)
    #calculate revealed value of success
    rev_value_vec <- rowSums(logical_matrix * value_vec_a * project_success)
    
    
    # vector of the true probabilities
    # make a vector of true_prob
    true_prob_v <- as.vector(true_prob)
    # make a true probability matrix representing the true probaility
    true_prob_matrix <- repmat(t(true_prob_v), nrow(logical_matrix), 1)
    # vector of the estimated probabilities
    true_prob_vec = rowSums(logical_matrix * true_prob_matrix)
    
    # vector of the estimated probabilities
    # est_prob_vec = sum(logical_matrix.*repmat(removal_prob_est(:)',size(logical_matrix,1),1),2)
    # #calculate estimated probability of success
    est_prob_vec <- rowSums(logical_matrix * est_value_vec_b)
    
    
    
    # find the options that have the highest value and under budget
    # true_opt = find(value_vec == max(value_vec(cost_vec<Budget)));
    # which cost_vec is less that the budget (makes a logical index)
    underbudget <- cost_vec < budget
    # find those in value_vec which are truely underbudget
    value_underbudget <- value_vec[underbudget]
    # find those in cost_Vec which are truely underbudget
    #this is the true option, the maximum value underbudget
    true_opt <- which(value_vec == max(value_underbudget))
    # if multiple options are equal, choose cheapest
    if (length(true_opt) > 1){
      # true_opt = true_opt[which.min(cost_underbudget[true_opt])]
      true_opt <- true_opt[which(cost_vec[true_opt] == min(cost_vec[true_opt]))[1]]
    }
    
    
    
    # find the best set of projects using the revealed information
    # rev_opt = find(rev_value_vec == max(rev_value_vec(cost_vec<Budget)))
    rev_value_underbudget <- rev_value_vec[underbudget]
    # this is the revealed option
    rev_opt <- which(rev_value_vec == max(rev_value_underbudget))
    # if multiple options are equal, randomly choose cheapest
    if (length(rev_opt) > 1){
      # rev_opt <-rev_opt(find(cost_vec(rev_opt)==min(cost_vec(rev_opt)),1))
      rev_opt <- rev_opt[which(cost_vec[rev_opt] == min(cost_vec[rev_opt]))[1]]
    }
    
    
    
    # find the best option using the estimates
    # exp_opt = find(est_value_vec == max(est_value_vec(est_cost_vec<Budget)))
    est_underbudget <- est_cost_vec < budget
    est_value_underbudget <- est_value_vec[est_underbudget]
    # this is the revealed option
    exp_opt <- which(est_value_vec == max(est_value_underbudget))
    # if multiple options are equal, choose cheapest
    if (length(exp_opt) > 1) {
      # exp_opt = exp_opt(find(cost_vec(exp_opt)==min(cost_vec(exp_opt)),1))
      exp_opt <- exp_opt[which(cost_vec[exp_opt] == min(cost_vec[exp_opt]))[1]]
    }
    
    # create an array that holds all of the important quantities, of which a
    # subset will be stored
    outputs <-cbind(est_value_vec,
                    value_vec,
                    rev_value_vec,
                    est_cost_vec,cost_vec,
                    est_value_vec_success,
                    value_vec_success,
                    est_prob_vec,
                    true_prob_vec,
                    rowSums(logical_matrix))
    # cols: 1. exp value, 2. true value, 3. exp. cost, 4. rev. cost, 5. exp val all success,
    # 6. rev val all success, 7. sum exp pr, 8. sum rev pr., 9 no. proj selected
    
    
    
    
    # PPP method
    # create a vector of weights
    ppp_weights <- removal_value_est * removal_prob_est / removal_cost_est
    # sort decisions by the weightings
    ppp_order <- order(ppp_weights)
    # calculate the cumulitive costs
    cumcosts <- rev(cumsum(rev(removal_cost_est[ppp_order])))
    # choose the projects which come under-budget
    start <- which(cumcosts < budget)[1]
    # create an index for ppp_order
    idx <- start:length(ppp_order)
    # create a vector of project numbers
    ppp_projects <- ppp_order[idx]
    # initialise a vector to store the projects
    ppp_actions <- matrix(0, 1, total_species)
    # set the chosen projects to 1
    ppp_actions[ppp_projects] <- 1
    # find which action set this corresponds to
    ppp_action_set <- which(row_member(logical_matrix, ppp_actions),1)
    
    
    # As above, but for cost benefit
    # create a vector of weights
    cost_ben_weights <- removal_value_est / removal_cost_est
    # sort decisions by the weightings
    cost_ben_order <- order(cost_ben_weights)
    
    # calculate the cumulitive costs
    cumcosts <- rev(cumsum(rev(removal_cost_est[cost_ben_order])))
    # choose the projects which come under-budget
    start <- which(cumcosts < budget)[1]
    # create an index for ppp_order
    idx <- start:length(cost_ben_order)
    # create a vector of project numbers
    cost_ben_projects <- cost_ben_order[idx]
    # initialise a vector to store the projects
    cost_ben_actions <- matrix(0, 1, total_species)
    # set the chosen projects to 1
    cost_ben_actions[cost_ben_projects] <- 1
    # find which action set this corresponds to
    cost_ben_action_set <- which(row_member(logical_matrix, cost_ben_actions),1)
    
    
    # Random selection
    #store remaining budget
    budget_rem <- budget
    flag <- 0
    # initalise a vector to store projects
    rand_action_vec = matrix(0, 1, total_species)
    
    # while under budget
    while (flag == 0){
      # find which projects are still affordable
      # under_budget = find((removal_cost_est(:)' < budget_rem) & ~rand_action_vec)
      under_budget <- which((as.vector(removal_cost_est) < budget_rem) & !rand_action_vec)
      
      # if (isempty(under_budget)){
      if (length(under_budget) == 0){
        # if nothing is affordable, then exit
        flag <- 1
        
      } else {
        # randomly add an affordable project
        new_project <-  under_budget[ceiling(unifrnd_2(0,1) * length(under_budget))]
        # set that project to 1 in the aciton set
        rand_action_vec[new_project] <- 1
        # remove cost from remaining budget
        budget_rem <- budget_rem - removal_cost_est[new_project]
        
      }
    }
    
    # find which action set this corresponds to
    rand_action_set <- which(row_member(logical_matrix, rand_action_vec),1)
    
    
    
    # store all of the chosen action sets from all of the methods
    actions_set <-cbind(rev_opt,
                        true_opt,
                        exp_opt,
                        ppp_action_set,
                        cost_ben_action_set,
                        rand_action_set)                
    
    # if being over-budget causes failures (set to TRUE or FALSE)                  
    if (over_budget_fail) {
      
      # check which action sets are over budget
      overbudget <- cost_vec > budget
      # get their indices
      overbudget_index <- which(overbudget)
      # store the number of projets in each action set
      n_projects <- rowSums(logical_matrix)
      # handle case where there are none overbudget
      if (any(overbudget)) {
        # the fewest actions that are overbudget
        min_proj_over <- min(n_projects[overbudget])
        # the most actions that are overbudget
        max_proj_over <- max(n_projects[overbudget])
        # seq to iterate over
        seq <- min_proj_over:max_proj_over
      } else {
        seq <- c()
      }
      
      # loop over the possible number of overbudget actions
      for (i in seq) {
        # find which action sets are overbudget and have i projects
        current_projects <- which(overbudget & (n_projects == i))
        # for each of these action sets
        for (j in 1:length(current_projects)) {
          
          # only proceed if it is one of the chosen action sets
          if (is_member(current_projects[j], actions_set)) {
            
            #     if (current_projects[j] == actions_set(1)) {
            #       # keyboard
            #     }
            
            # find which projects were chosen
            project_choices <- logical_matrix[current_projects[j], , drop = FALSE]
            # randomly
            project_choices[, randsample_2(which(project_choices == 1))] <- 0
            # find the new index for the set of projects
            new_val_index <- which(row_member(logical_matrix, project_choices))
            
            while (cost_vec[new_val_index] > budget) {
              # if the true cost is still above the dubdget, continue removing projects
              project_choices <- logical_matrix[new_val_index, , drop = FALSE]
              project_choices[, randsample_2(which(project_choices == 1))] <- 0
              new_val_index = which(row_member(logical_matrix, project_choices))
            }
            
            # update true value of the project set
            value_vec[current_projects[j]] <- value_vec[new_val_index]
            # update the revealed value of the project set
            rev_value_vec[current_projects[j]] <- rev_value_vec[new_val_index]
          }
        }
      }
      
    } else {
      # scale the final value by the amount overbudget
      if (no_value_reduction == FALSE) {
        # initialise a vector of ones
        scaling <- matrix(1, dim(cost_vec))
        # calculate the % overbudget acion sets are
        scaling[cost_vec > budget] <- budget / cost_vec[cost_vec > budget]
        # reduce the value if overbudget
        value_vec <- value_vec * scaling
        # reduce the value if overbudget
        rev_value_vec <- rev_value_vec * scaling
      }
    }
    
    
    # update the outputs with the new values
    outputs <- cbind (est_value_vec,
                      value_vec,
                      rev_value_vec,
                      est_cost_vec,
                      cost_vec,
                      est_value_vec_success,
                      value_vec_success,
                      est_prob_vec,
                      true_prob_vec,
                      rowSums(logical_matrix))
    
    
    # again, in Matlab enter debug mode if any of the outputs are NANs
    # if any(isnan(outputs(:))){
    #   keyboard
    # }
    
    
    # store the data for each different strategy
    rand_sel_array[r, ] <- outputs[rand_action_set, ]
    cost_ben_array[r, ] <- outputs[cost_ben_action_set, ]
    ppp_array[r, ] <- outputs[ppp_action_set, ]
    rev_opt_array[r, ] <- outputs[rev_opt, ]
    true_opt_array[r, ] <- outputs[true_opt, ]
    exp_opt_array[r, ] <- outputs[exp_opt, ]
    
    # end of giant while loop!
    
  }
  
  
  # output of the data files
  # vector of column names
  column_names <- c( "expected.val", "true.expected.val", "true.expected.val",
                     "expected.cost", "revealed.cost", "expected.val.all.success",
                     "revealed.val.all.success", "summed.expected.prob", "summed.revealed.prob", "num.projects.selected")
  
  
  # 'expected.val', # sum of expected benefits multiplied by the expected prob of succsess for all projectes selected
  # 'true.expected.val', # sum of the true benefits x true prob of success for all selected projects
  # 'revealed.val', # true outcome: sum of the true benefits of all successful projects (given have rolled dice and see which ones failed)
  # 'expected.cost', # These don't incoporate success of failure, just expected cost
  # 'revealed.cost', # true cost of all selected projects
  # 'expected.val.all.success', # estimated value of all projects selected (assuming all successful)
  # 'revealed.val.all.success', # true value of all projects selected (assuming all successful)
  # 'summed.expected.prob',  # Sum of expected prob of success for all projects selected
  # 'summed.revealed.prob',  # Sum of revealed prob of success for all projects selected
  # 'num.projects.selected'
  
  #give each object the correct column names
  colnames(rand_sel_array) <- column_names
  colnames(cost_ben_array) <- column_names
  colnames(ppp_array) <- column_names
  colnames(rev_opt_array) <- column_names
  colnames(true_opt_array) <- column_names
  colnames(exp_opt_array) <- column_names
  
  
  # calculate means
  rev_opt_mean <- mean(rev_opt_array)
  true_opt_mean <- mean(true_opt_array)
  exp_opt_mean <- mean(exp_opt_array)
  cost_ben_mean <- mean(cost_ben_array)
  ppp_mean <- mean(ppp_array)
  rand_mean <- mean(rand_sel_array)
  
  
  # write the output to file
  # folder name for outputs
  outfolder <- 'outputs'
  # create a folder of this name
  dir.create(outfolder, showWarnings = FALSE)
  
  
  #create a general suffix to show the input variables for this run
  fileSuffix <- paste0(n_islands,
                       "_nsp",
                       n_species,
                       "_rep",
                       reps,
                       ".csv")
  
  #rand_sel_array
  fname <- paste0("random_allocation_nisl", fileSuffix)
  write.csv(rand_sel_array, file = paste(outfolder, fname, sep= '/'), row.names = FALSE)
  # ppp_array
  fname <- paste0("ppp_allocation_nisl", fileSuffix)
  write.csv(ppp_array, file = paste(outfolder, fname, sep= '/'), row.names = FALSE)
  # cost_ben_array
  fname <- paste0("cost_ben_allocation_nisl", fileSuffix)
  write.csv(cost_ben_array, file = paste(outfolder, fname, sep= '/'), row.names = FALSE)
  # exp_opt_array
  fname <- paste0("opt_allocation_nisl", fileSuffix)
  write.csv(exp_opt_array, file = paste(outfolder, fname, sep= '/'), row.names = FALSE)
  # true_opt_array
  fname <- paste0("opt_allocation_true_nisl", fileSuffix)
  write.csv(true_opt_array, file = paste(outfolder, fname, sep= '/'), row.names = FALSE)
  # rev_opt_array
  fname <- paste0("opt_allocation_revealed_nisl", fileSuffix)
  write.csv(rev_opt_array, file = paste(outfolder, fname, sep= '/'), row.names = FALSE)
  
 }




# run simulation function
simulation()

# time running of simulation function
system.time(simulation())

# profile the code to see which bits are slow
# # install.packages("profvis")
# library (profvis)
# profvis(simulation())
