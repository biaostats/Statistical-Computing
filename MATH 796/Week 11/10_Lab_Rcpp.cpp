#include <iostream>
#include <queue>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerVector which_max_cpp(NumericVector x) {
	int max;
	std::queue<int> indecies;
	/* Use queue to store the max indecies. I try
	  to not to use any function here, only use
	  the data structure to find all max indecies,
	  and the time complexity is resitricted to O(N).*/
	for (int i = 0; i < x.size(); ++i) {
		if (!indecies.empty()) {
			if(x[i] > max) {
				while (!indecies.empty()) {
					indecies.pop();
				}
				max = x[i];
				indecies.push(i+1);
			} else if (x[i] == max) {
				indecies.push(i+1);
			}
		} else if (indecies.empty()) {
			max = x[i];
			indecies.push(i+1);
		}
	}
	/*I'm not sure whether Rcpp could accept the return type as queue,
	  since queue is the data structure in c++. I move all results to 
	  IntegerVector then return it.*/
	IntegerVector result = seq_len(indecies.size());
	int i = 0;
	while (!indecies.empty()){
		result[i] = indecies.front();
		indecies.pop();
		i++;
	}
	return result;
}

double f_quad_cpp(arma::mat x, double a){
	double result = x(0,0) + 2*a*x(0,0)*x(1,0) + x(1,0)*x(1,0);
	return result;
}

arma::mat f_grad_quad_cpp(arma::mat x, double a){
	arma::mat result(2,1);
	result(0,0) = 2*x(0,0) + 2*a*x(1,0);
	result(1,0) = 2*a*x(0,0) + 2*x(1,0);
	return result; 
}

arma::mat f_hess_quad_cpp(double a){
	arma::mat result(2,2);
	result(0,0) = 2;
	result(0,1) = 2*a;
	result(1,0) = 2*a;
	result(1,1) = 2;
	return result;
}

// [[Rcpp::export]]
arma::mat update_newton_cpp(arma::mat x, double a) {
	arma::mat result = x - arma::solve(f_hess_quad_cpp(a),f_grad_quad_cpp(x,a));
	return result;
}

// [[Rcpp::export]]
NumericVector ar_sampling_exp_cpp(int n) {
	int c = 2, k = 0;
	NumericVector x_exp(n), u(1), x(1);
	while (k < n) {
		u = runif(1);
		x = rchisq(1,1);
		if (dexp(x)[0]/(dchisq(x,1.0)[0]*c) > u[0]) {
			x_exp[k] = x[0];
			k++;
		}		
	}
	return x_exp;
}
