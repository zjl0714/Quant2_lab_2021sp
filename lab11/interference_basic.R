rm(list=ls())
devtools::install_github('szonszein/interference')
library(interference)

(adj_matrix <- make_adj_matrix(N = 9, model = 'sq_lattice'))


(tr_vector <- make_tr_vec_permutation(N = 9, p = 0.2,
                                     R = 1, seed = 357))

(obs_exposure <- make_exposure_map_AS(adj_matrix, tr_vector,
                                     hop = 1))

# Simulate a vector of outcome data:


# Rosenbaum Dialated effect: 
# y11 = 2*y00, y10= 1.5*y00, y01=1.25*y00 

(potential_outcome <- make_dilated_out(adj_matrix, make_corr_out,
                                      seed = 357, hop = 1))

obs_outcome <- rowSums(obs_exposure*t(potential_outcome))

# Create exposure probabilities:

potential_tr_vector <- make_tr_vec_permutation(N = 9, p = 0.2,
                                                   R = 36,
                                                   seed = 357)
# why 36? all combination c(9,2)
choose(9,2)
 
obs_prob_exposure <- make_exposure_prob(potential_tr_vector, 
                                        adj_matrix, 
                                        make_exposure_map_AS, 
                                        list(hop=1))
 
# Estimate exposure-specific causal effects and their variance:

estimates(obs_exposure, obs_outcome, obs_prob_exposure,
          n_var_permutations = 30,
          hop = 1)

# Create adjacency matrix and treatment vector to
# produce observed exposure conditions according to the
# "full neighborhood" exposure mapping:

adj_matrix <- make_adj_matrix(N = 81, model = 'sq_lattice')

tr_vector <- make_tr_vec_permutation(N = 81, p = 0.6,
                                     R = 1, seed = 3579)

obs_exposure_full_nei <- make_exposure_map_full_neighborhood(adj_matrix,
                                                             tr_vector)
# Simulate a vector of outcome data:

potential_outcome_full_nei <-
    make_dilated_out_full_neighborhood(adj_matrix, make_corr_out,
                                       seed = 357)

obs_outcome_full_nei <-
    rowSums(obs_exposure_full_nei*t(potential_outcome_full_nei))

# Create exposure probabilities:

potential_tr_vector <- make_tr_vec_permutation(N = 81, p = 0.5,
                                               R = 100,
                                               seed = 357)
# due to computation constraint
# we cannot take R to the true combination number
#

obs_prob_exposure_full_nei <- make_exposure_prob(potential_tr_vector,
                                                 adj_matrix,
                                                 make_exposure_map_full_neighborhood)

# Estimate exposure-specific causal effects and their variance:

estimators_full_neighborhood(obs_exposure_full_nei, obs_outcome_full_nei,
                             obs_prob_exposure_full_nei,
                             n_var_permutations = 50)


# More examples in the book chapter
# Aronow, P.M. et al. (2020). 
# Spillover effects in experimental data. 
# https://arxiv.org/abs/2001.05444
