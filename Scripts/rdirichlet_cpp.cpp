// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::mat rdirichlet_cpp(int num_samples,
                         arma::vec alpha_m) {
    int distribution_size = alpha_m.n_elem;
    // each row will be a draw from a Dirichlet
    arma::mat distribution = arma::zeros(num_samples, distribution_size);

    for (int i = 0; i < num_samples; ++i) {
        double sum_term = 0;
        // loop through the distribution and draw Gamma variables
        for (int j = 0; j < distribution_size; ++j) {
            double cur = R::rgamma(alpha_m[j],1.0);
            distribution(i,j) = cur;
            sum_term += cur;
        }
        // now normalize
        for (int j = 0; j < distribution_size; ++j) {
            distribution(i,j) = distribution(i,j)/sum_term;
        }
    }
    return(distribution);
}
