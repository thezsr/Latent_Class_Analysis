data {
  int<lower = 1> NI; //number of items
  int<lower = 1> NC; //number of classes
  int<lower = 1> N; //number of participants
  array[N, NI] int<lower = 0, upper = 1> Y; //observations
}

parameters {
  simplex[NC] alpha; //marginal probabilities for classes
  //conditional probabilities of endorsing item I given class C
  array[NC, NI] real<lower = 0, upper = 1> p; 
  
}

transformed parameters {
  vector[NI] prod_p; //product of probs across classes for each item
}