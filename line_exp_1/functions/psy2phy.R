psy2phy <- function(n) {
  return(150*1.05^n)
}
phy2psy <- function(a) {
  log(a/150,1.05)
}