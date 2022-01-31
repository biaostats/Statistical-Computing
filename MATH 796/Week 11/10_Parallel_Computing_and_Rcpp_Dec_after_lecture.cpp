#include <functional>
#include <iostream>
#include <Rcpp.h>
using namespace Rcpp;


double secant_update_example(double x_init0,double x_init1){
  double f0 = x_init0-log(abs(x_init0)+1)-1;
  double f1 = x_init1-log(abs(x_init1)+1)-1;
  double x_new = x_init1 - f1*(x_init1-x_init0)/(f1-f0);
  return(x_new);
}

// [[Rcpp::export]]
List secantList_example(double x_init0,double x_init1, double err_max, int iter_max) {
  List output;
  double x_new;
  int iter=0;
  double err = 100*err_max;
  do{
    x_new = secant_update_example(x_init0,x_init1);
    err = abs(x_new-x_init1);
    x_init0 = x_init1;
    x_init1 = x_new;
    iter++;
  }
  while(err>err_max&&iter<iter_max);
  output.push_back(x_new,"solution");
  output.push_back(iter,"iteration");
  output.push_back(err<err_max,"convergence");
  return output;
}



double f_example(double x){
  double output = x-log(abs(x)+1)-1;
  return output;
}


double secant_update(double x_init0,double x_init1, double(*func)(double)){
  double f0 = func(x_init0);
  double f1 = func(x_init1);
  double x_new = x_init1 - f1*(x_init1-x_init0)/(f1-f0);
  return(x_new);
}


// [[Rcpp::export]]
List secantList(double x_init0,double x_init1, double err_max, int iter_max) {
  List output;
  double x_new;
  int iter=0;
  double err = 100*err_max;
  do{
    x_new = secant_update(x_init0,x_init1, &f_example);
    err = abs(x_new-x_init1);
    x_init0 = x_init1;
    x_init1 = x_new;
    iter++;
  }
  while(err>err_max&&iter<iter_max);
  output.push_back(x_new,"solution");
  output.push_back(iter,"iteration");
  output.push_back(err<err_max,"convergence");
  return output;
}

// [[Rcpp::export]]
double mycor(NumericVector x, NumericVector y, int n){
  double output = (std::inner_product(x.begin(), x.end(), y.begin(), 0.)-mean(x)*mean(y)*n)/((n-1)*sd(x)*sd(y));
  return output;
}

// [[Rcpp::export]]
List boot_cor_onerep(NumericVector x, NumericVector y, IntegerVector idall, int n){
  IntegerVector idi =sample(idall,n,TRUE);
  NumericVector xi =x[idi-1];
  NumericVector yi =y[idi-1];
  double cori = mycor(xi,yi,n);
  List output;
  output.push_back(cori,"stat");
  output.push_back(idi,"sample_id");
  return output;
}


// [[Rcpp::export]]
List boot_cor_main(NumericVector x, NumericVector y, int B){
  int n = x.size();
  IntegerMatrix id(n,B);
  NumericVector stat(B);
  IntegerVector idall = seq_len(n);
  for(int i = 0; i < B; i++){
    List outi = boot_cor_onerep(x,y,idall,n);
    stat[i] = outi["stat"];
    IntegerVector idi=outi["sample_id"];
    id(_,i) = idi;
  }
  List output;
  output.push_back(stat,"stat");
  output.push_back(id,"sample_id");
  return output;
}
