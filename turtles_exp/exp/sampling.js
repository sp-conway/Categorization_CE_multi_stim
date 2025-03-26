// sampling.js
// functions for randomly sampling from distributions in JavaScript
// Sean Conway
// Last modified Dec. 2021

// runif()
// sample from continuous uniform distribution
function runif (min, max){
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min) + min)
  return samp
}

// rnorm()
// sample from normal distribution
function rnorm (n, mu, sigma){
  let samp = [];
  for(i=0; i<n; i++){
    samp.push(Math.sqrt(-2*Math.log(Math.random())) * Math.cos(2*Math.PI*Math.random()))
  }
  let samp_scaled = [];
  for(i=0; i<n; i++){
    samp_scaled.push((samp[i]*sigma)+mu)
  }
  return samp_scaled
}
