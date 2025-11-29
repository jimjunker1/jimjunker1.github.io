data {
  int<lower=1> K;        // Number of communities
  int<lower=1> S;        // Number of species
  matrix[K, S] n;        // Abundance matrix (K x S)
  vector[K] M;           // Observed total metabolic rates for each community
}

parameters {
  vector[S] alpha;              // Baseline metabolic rates for each species
  matrix[S, S] a;               // Interaction terms (S x S matrix)
  real<lower=0> sigma;          // Error standard deviation
}

model {
  vector[K] M_hat;              // Predicted metabolic rates

  // Priors (you may adjust based on prior knowledge)
  alpha ~ normal(0, 1);
  to_vector(a) ~ normal(0, 1);
  sigma ~ normal(0, 1);

  // Likelihood
  for (k in 1:K) {
    M_hat[k] = 0;
    for (i in 1:S) {
      real m_i_k = alpha[i];
      for (j in 1:S) {
        m_i_k += a[i,j] * n[k, j];
      }
      M_hat[k] += m_i_k * n[k, i];
    }
  }
  M ~ normal(M_hat, sigma);
}
