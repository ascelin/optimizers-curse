n_islands = 3;      %number of islands
n_species = 4;
total_species = n_islands*n_species;
Budget = 2e7;       %project budget
reps = 10000;          %number of trials
cost_unceratinty = true;
over_budget_fail = true; % choose random erad to fail if over budget 

% Only has effect when overbudget_fail = false. If false then it will reduce the total benefit of projects proportional to the 
% amount you're over budget. Note code checks for over_budget_fail first,
% only checks fir this if over_budget_fail = false; 
no_value_reduction = true; 

interaction_flag = false; % note see disc_param needs to be set to something other than [1,1] for this to work.
failure_prob = true;  % if false, then prob failure set to 0 and B:C and PPP are identical. If true then have prob of failure for each project.
prob_uncerainty = true; % This only has an effect when failure_prob = true; This determine whether there is uncertainty in the prob of failure


% range for discount for multiple spp on a single island 
disc_param = [.7,.9];
% e.g. to make a 20% discount would be disc_param = [0.8,0.8];
% and disc_param = [0.7,0.8]; discount randomly would range from between 70% and 80%

% % param generation

%true cost - lognormal distribution
cost_mu = 1.25;
cost_sigma = 0.5;

%true probability of success - beta distribution
prob_shape = 20; %smaller = more variation. ~5 for lots of variation, ~30-50 for minimal variation
prob_dist_mean = .75;  %average probability of success

%estimated cost & value - normal distribution 
est_mu = 1;
est_sigma = 0.5; % bigger -> more variation

%estimated probability of success & discount amount - beta distribution
shape_prob = 4; %smaller = more variation. ~5 for lots of variation, ~30-50 for minimal variation

% Arrays to store the decisions for each of the different methods
rev_opt_array = zeros(reps,10); %optimal choice based on the revealed values (after projects succeed/fail)
true_opt_array = rev_opt_array; %optimal choice based on the true values
exp_opt_array = rev_opt_array; %  choice using the expected values
cost_ben_array = exp_opt_array; %  choice using benefit/cost
ppp_array = exp_opt_array; % choice using PPP
rand_sel_array = exp_opt_array; % random

%create a matrix whose columns are every possible combination of
%2^total_species 1's and 0's. These represent every possible decision
logical_matrix = zeros(2^total_species,total_species);

for i = 1:total_species
    new_col = [];
    while length(new_col) < 2^total_species
        new_col = [new_col;ones(2^(i-1),1);zeros(2^(i-1),1)];
    end
    logical_matrix(:,i) = new_col;
end
logical_matrix = fliplr(logical_matrix);

disc_array = zeros(2^total_species,total_species); %initialise an array that records which projects get cost discounts, given a decision

for i = 1:size(disc_array,1)
    for j = 1:n_species:n_islands*n_species
        disc_array(i,j:j+n_species-1) = sum(logical_matrix(i,j:j+n_species-1))>1; %if two or more projects on that island has been chosen, then all projects are given the discount
    end
end

r = 0;

while r < reps
    r = r+1;
    r/reps
    
    %generate true values
    true_cost = unifrnd(3,6,n_islands,n_species)*1e6; % true cost between 3 and 6 million
    true_disc = unifrnd(disc_param(1),disc_param(2),n_islands,n_species); %discout parameter form uniform distribution
    true_value = unifrnd(1,2,n_islands,n_species); % true value from between 0 and 1
    
   
    
    if ~failure_prob
        true_prob(:) = 1; % projects always succeed
    else
        true_prob = unifrnd(.2,.8,[n_islands,n_species]); % generatee true probability of success from .2 to .8.
    end
    
    proj_succ = true_prob > rand(size(true_prob)); % draw wheather each project is successfull or not
    

    if cost_unceratinty == false % check if there is cost uncertainty or not
        removal_cost_est = true_cost; % if not, the estiamted cost is the true cost
    else
        removal_cost_est = true_cost+(unifrnd(-1.5,1.5,n_islands,n_species))*1e6; % estimate the cost from +- 1.5 million around the true value
    end

    removal_value_est = true_value+unifrnd(-1,1,[n_islands,n_species]); % estiamte the value from +-1 around the true value (hence estimated values can be between 0 and 3)
    
    
    if interaction_flag % check if there are interactions
        unc_range = min([disc_param(1),1-disc_param(2)]); % set the maximum range of uncertainty such that discount_est will never be < 0 or > 1.
        removal_discount_est = true_disc + unifrnd(-unc_range,unc_range,n_islands,n_species); %draw remaoval discount est from a unifrom distribution
    else
        removal_discount_est = true_disc; %if no interactions, set removal discount estimate to be the true discout.
    end
    
    if prob_uncerainty
        % this is the probability of sucess uncertainty. 
        removal_prob_est = true_prob + unifrnd(-.2,.2,n_islands,n_species); %draw from a uniform distribution
    else
        removal_prob_est = true_prob; %if no uncertainty, set equal to true value.
    end
    
    cost_matrix = logical_matrix.*repmat(true_cost(:)',size(logical_matrix,1),1).*(repmat(true_disc(:)',size(logical_matrix,1),1).*disc_array + ~disc_array); %calculate the true cost of every project, including the discount
    est_cost_matrix = logical_matrix.*repmat(removal_cost_est(:)',size(logical_matrix,1),1).*(repmat(removal_discount_est(:)',size(logical_matrix,1),1).*disc_array + ~disc_array);%calculate the estimated cost of every project, including the discount
    cost_vec = sum(cost_matrix,2); %cacluate the true cost of every set of projects
    est_cost_vec = sum(est_cost_matrix,2); %cacluate the estimated cost of every set of projects
    
    value_vec = sum(logical_matrix.*repmat(true_value(:)',size(logical_matrix,1),1).*repmat(true_prob(:)',size(logical_matrix,1),1),2); %create a vector that stores the true value for each option, weighted by probability of success
    est_value_vec = sum(logical_matrix.*repmat(removal_value_est(:)',size(logical_matrix,1),1).*repmat(removal_prob_est(:)',size(logical_matrix,1),1),2);%create a vector that stores the estimated value for each option, weighted by probability of success
    value_vec_success = sum(logical_matrix.*repmat(true_value(:)',size(logical_matrix,1),1),2); %create a vector that stores the true value for each option, assuming all successfull
    est_value_vec_success = sum(logical_matrix.*repmat(removal_value_est(:)',size(logical_matrix,1),1),2); %create a vector that stores the estimated value for each option, assuming all successfull
    rev_value_vec = sum(logical_matrix.*repmat(true_value(:)',size(logical_matrix,1),1).*repmat(proj_succ(:)',size(logical_matrix,1),1),2); %create a vector that stores the revealed value for each option
    true_prob_vec = sum(logical_matrix.*repmat(true_prob(:)',size(logical_matrix,1),1),2); %vector of the true probabilities
    est_prob_vec = sum(logical_matrix.*repmat(removal_prob_est(:)',size(logical_matrix,1),1),2); %vector of the estimated probabilities
    
    
    
    true_opt = find(value_vec == max(value_vec(cost_vec<Budget))); % find the option that has the highest value and under budget
    if length(true_opt) > 1 % if multiple options are equal, choose cheapest
        true_opt = true_opt(find(cost_vec(true_opt)==min(cost_vec(true_opt)),1));
    end
    rev_opt = find(rev_value_vec == max(rev_value_vec(cost_vec<Budget))); %find the best set of projects using the revealed information
    if length(rev_opt) > 1 % if multiple options are equal, randomly choose cheapest
        rev_opt = rev_opt(find(cost_vec(rev_opt)==min(cost_vec(rev_opt)),1));
    end
    exp_opt = find(est_value_vec == max(est_value_vec(est_cost_vec<Budget))); %find the best option using the estimates
    if length(exp_opt) > 1 % if multiple options are equal, choose cheapest
        exp_opt = exp_opt(find(cost_vec(exp_opt)==min(cost_vec(exp_opt)),1));
    end
    
    %create an array that holds all of the important quantities, of which a
    %subset will be stored
    outputs = [est_value_vec,value_vec,rev_value_vec,est_cost_vec,cost_vec,est_value_vec_success,value_vec_success,est_prob_vec,true_prob_vec,sum(logical_matrix,2)];
    %cols: 1. exp value, 2. true value, 3. exp. cost, 4. rev. cost, 5. exp val all success, 
    %6. rev val all success, 7. sum exp pr, 8. sum rev pr., 9 no. proj selected
    
    
    % PPP method
    
    ppp_weights = removal_value_est.*removal_prob_est./removal_cost_est; %create a vector of weights
    [~,ppp_order] = sort(ppp_weights(:)); %sort decisions by the weightings
    cumcosts = flipud(cumsum(flipud(removal_cost_est(ppp_order)))); %calculate the cumulitive costs
    ppp_projects = ppp_order(find(cumcosts<Budget,1):end); %choose the projects which come under-budget
    ppp_actions = zeros(1,total_species); %initialise a vector to store the projects
    ppp_actions(ppp_projects) = 1; %set the chosen projects to 1
    ppp_action_set = find(ismember(logical_matrix,ppp_actions,'rows'),1); %find which action set this corresponds to
    
    %As above, but for cost benefit
    cost_ben_weights = removal_value_est./removal_cost_est;
    [~,cost_ben_order] = sort(cost_ben_weights(:));
    cumcosts = flipud(cumsum(flipud(removal_cost_est(cost_ben_order))));
    cost_ben_projects = cost_ben_order(find(cumcosts<Budget,1):end);
    cost_ben_actions = zeros(1,total_species);
    cost_ben_actions(cost_ben_projects) = 1;
    cost_ben_action_set = find(ismember(logical_matrix,cost_ben_actions,'rows'),1);
    
    
    % Random selection
    budget_rem = Budget; %store remaining budget
    flag = 0;
    rand_action_vec = zeros(1,total_species); %initalise a vector to store projects
    while flag == 0 %while under budget
        under_budget = find((removal_cost_est(:)' < budget_rem) & ~rand_action_vec); %find which projects are still affordable
        if isempty(under_budget)
            flag = 1; %if nothing is affordable, then exit
        else
        new_project = under_budget(ceil(rand*length(under_budget))); %randomly add an affordable project
        rand_action_vec(new_project) = 1; %set that project to 1 in the aciton set
        budget_rem = budget_rem - removal_cost_est(new_project); %remove cost from remaining budget
        end
    end
    rand_action_set = find(ismember(logical_matrix,rand_action_vec,'rows'),1);%find which action set this corresponds to
    
    
    actions_set = [rev_opt,true_opt,exp_opt,ppp_action_set,cost_ben_action_set,rand_action_set]; %store all of the chosen action sets from all of the methods    
    if over_budget_fail %if being over-budget causes failures
        overbudget = cost_vec>Budget; %check which action sets are over budget
        overbudget_index = find(overbudget); % get their indices 
        n_projects = sum(logical_matrix,2); %store the number of projets in each action set
        min_proj_over = min(n_projects(overbudget)); % the fewest actions that are overbudget
        max_proj_over = max(n_projects(overbudget)); % the most actions that are overbudget
        for i = min_proj_over:max_proj_over %loop over the possible number of overbudget actions
            current_projects = find(overbudget & (n_projects==i)); % find which action sets are overbudget and have i projects
            for j = 1:length(current_projects) % for each of these action sets
                if ismember(current_projects(j),actions_set) % only proceed if it is one of the chosen action sets
                    if current_projects(j) == actions_set(1)
                        keyboard
                    end
                    project_choices = logical_matrix(current_projects(j),:); %find which projects were chosen
                    project_choices(randsample(find(project_choices==1),1)) = 0; %randomly 
                    new_val_index = find(ismember(logical_matrix,project_choices,'rows')); %find the new index for the set of projects
                    while cost_vec(new_val_index) > Budget % if the true cost is still above the dubdget, continue removing projects
                        project_choices = logical_matrix(new_val_index,:);
                        project_choices(randsample(find(project_choices==1),1)) = 0;
                        new_val_index = find(ismember(logical_matrix,project_choices,'rows'));
                    end
                    value_vec(current_projects(j)) = value_vec(new_val_index); % update true value of the project set
                    rev_value_vec(current_projects(j)) = rev_value_vec(new_val_index); % update the revealed value of the project set
                end
            end
        end
    else %scale the final value by the amount overbudget
        if no_value_reduction == false
            scaling = ones(size(cost_vec)); %initialise a vector of ones
            scaling(cost_vec>Budget) = Budget./cost_vec(cost_vec>Budget); % calculate the % overbudget acion sets are 
            value_vec = value_vec.*scaling; % reduce the value if overbudget
            rev_value_vec = rev_value_vec.*scaling; % reduce the value if overbudget
        end
    end
    %update the outputs with the new values
    outputs = [est_value_vec,value_vec,rev_value_vec,est_cost_vec,cost_vec,est_value_vec_success,value_vec_success,est_prob_vec,true_prob_vec,sum(logical_matrix,2)];
    if any(isnan(outputs(:)))
        keyboard
    end
    %store the data for each different strategy
    rand_sel_array(r,:) = outputs(rand_action_set,:);
    cost_ben_array(r,:) = outputs(cost_ben_action_set,:);
    ppp_array(r,:) = outputs(ppp_action_set,:);
    rev_opt_array(r,:) = outputs(rev_opt,:);
    true_opt_array(r,:) = outputs(true_opt,:);
    exp_opt_array(r,:) = outputs(exp_opt,:);
    
% 
%     if rev_opt_array(r,:) == 0
%         keyboard
%     end

end

%scale all of the values by the performance of the revealed optimal choice
true_opt_array(:,[1,3,6,7]) = true_opt_array(:,[1,3,6,7])./repmat(rev_opt_array(:,6),1,4);
exp_opt_array(:,[1,3,6,7]) = exp_opt_array(:,[1,3,6,7])./repmat(rev_opt_array(:,6),1,4);
cost_ben_array(:,[1,3,6,7]) = cost_ben_array(:,[1,3,6,7])./repmat(rev_opt_array(:,6),1,4);
ppp_array(:,[1,3,6,7]) = ppp_array(:,[1,3,6,7])./repmat(rev_opt_array(:,6),1,4);
rand_sel_array(:,[1,3,6,7]) = rand_sel_array(:,[1,3,6,7])./repmat(rev_opt_array(:,6),1,4);
rev_opt_array(:,[1,3,6,7]) = rev_opt_array(:,[1,3,6,7])./repmat(rev_opt_array(:,6),1,4);

% calculate means
rev_opt_mean = mean(rev_opt_array);
true_opt_mean = mean(true_opt_array);
exp_opt_mean = mean(exp_opt_array);
cost_ben_mean = mean(cost_ben_array);
ppp_mean = mean(ppp_array);
rand_mean = mean(rand_sel_array);

figure(1)
clf
hold on
plot(true_opt_mean(1),true_opt_mean(2),'o')
plot(exp_opt_mean(1),exp_opt_mean(2),'o')
plot(ppp_mean(1),ppp_mean(2),'o')
plot(cost_ben_mean(1),cost_ben_mean(2),'o')
plot(rand_mean(1),rand_mean(2),'o')
xl = xlim;
yl = ylim;
xlim(xl)
ylim(yl)
plot([0,1],[0,1],'--')
xlabel('expected performance')
ylabel('true performance')
legend('true opt','exp opt','ppp','cost ben','rand','location','best')
% rev_opt_array = zeros(reps,9);
% true_opt_array = rev_opt_array;
% exp_opt_array = rev_opt_array;
% cost_ben_array = exp_opt_array;
% ppp_array = exp_opt_array;
% rand_sel_array = exp_opt_array;
 %cols: 1. exp value, 2. true value, 3. exp. cost, 4. rev. cost, 5. exp val all success, 
    %6. rev val all success, 7. sum exp pr, 8. sum rev pr., 9 no. proj selected
    
figure(2)
clf
hold on
plot(true_opt_mean(3),true_opt_mean(4),'o')
plot(exp_opt_mean(3),exp_opt_mean(4),'o')
plot(ppp_mean(3),ppp_mean(4),'o')
plot(cost_ben_mean(3),cost_ben_mean(4),'o')
plot(rand_mean(3),rand_mean(4),'o')
xl = xlim;
yl = ylim;
xlim(xl)
ylim(yl)
plot([0,max([xl,yl])],[0,max([xl,yl])],'--')
xlabel('expected cost')
ylabel('true cost')
legend('true opt','exp opt','ppp','cost ben','rand','location','best')

figure(3)
clf
hist(ppp_array(:,3)./ppp_array(:,4),50)
hold on
plot([0,0]+1,ylim,'--')
title('PPP cost surprise')

%write the output to file
csvwrite(['random_allocation_nisl',num2str(n_islands),'_nsp',num2str(n_species),'_rep',num2str(reps),'.csv'],rand_sel_array)
csvwrite(['ppp_allocation_nisl',num2str(n_islands),'_nsp',num2str(n_species),'_rep',num2str(reps),'.csv'],ppp_array)
csvwrite(['cost_ben_allocation_nisl',num2str(n_islands),'_nsp',num2str(n_species),'_rep',num2str(reps),'.csv'],cost_ben_array)
csvwrite(['opt_allocation_nisl',num2str(n_islands),'_nsp',num2str(n_species),'_rep',num2str(reps),'.csv'],exp_opt_array)
csvwrite(['opt_allocation_true_nisl',num2str(n_islands),'_nsp',num2str(n_species),'_rep',num2str(reps),'.csv'],true_opt_array)
csvwrite(['opt_allocation_revealed_nisl',num2str(n_islands),'_nsp',num2str(n_species),'_rep',num2str(reps),'.csv'],rev_opt_array)

