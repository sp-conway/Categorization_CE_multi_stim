var in_cat_trans = [];
for(i=1; i<in_category.length; i++){
  in_cat_trans.push(psy2phy(in_category[i]))
}

var out_cat_trans = [];
for(i=1; i<out_category.length; i++){
  out_cat_trans.push(psy2phy(out_category[i]))
}
console.log("In category lines = ", in_cat_trans)
console.log("Out category lines = ", out_cat_trans)
