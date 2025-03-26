function psy2phy (psy){
  let phy = 150*Math.pow(1.05,psy)
  return phy
}

function sample_in (min=0, max=22.5){
  let psy = Math.random()*(max-min)
  return psy
}

function sample_out (mu=11.25, sigma=2.25){
  let n1 = Math.random()
  let n2 = Math.random()
  let psy = Math.sqrt(-2*Math.log(n1)) * Math.cos(2*Math.PI*n2)
  let psy_scaled = (psy*sigma)+mu
  return psy_scaled
}
