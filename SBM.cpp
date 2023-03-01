
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <random>
using namespace Rcpp;
using namespace std;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// Define matrix type
typedef std::vector< std::vector<double> > matrix;

// [[Rcpp::export]]
std::vector<double> rnormCpp(int n = 1000, double mu = 0, double sigma = 1)
{
  // Création d'un générateur de nombres aléatoires suivant la loi normale
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<double> dist(mu, sigma);

  // Génération des nombres aléatoires et stockage dans un vecteur
  std::vector<double> values(n);
  for (int i = 0; i < n; i++) {
    values[i] = dist(gen);
  }

  return values;
}

// [[Rcpp::export]]
vector<double> cumsumCpp(vector<double> v){
  double cum = 0;
  for(int i = 0; i < v.size(); i++){
    cum += v[i];
    v[i] = cum;
  }
  return v;
}




