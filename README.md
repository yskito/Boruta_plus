# Project: Boruta_plus
This repository is related to Boruta package in R.

Now, I'm developing 3 thinngs below;
(1) Boruta + Random forest vs Random forest alone  
(2) Boruta + Logistic regression vs Logistic regression alone  
(3) Boruta + Multiple regression vs Multiple regression alone  

# Boruta packages
Boruta is a so powerful package to improve and simplify dataset, as it's a novel feature selection algorithm. 
See Miron B Kursa and Witold R. Rundnicki, "Feature Selection with the Boruta Package", Journal of Statistical Software, 2010. 
https://www.jstatsoft.org/article/view/v036i11


# folder: sample1_adult
data: http://archive.ics.uci.edu/ml/datasets/Adult
aim : to determine whether a person makes over 50K(salary) a year.
mthd: (1) Boruta + Random forest vs (2) Random forest alone
rslt: no difference between (1) and (2). -> all attributes are meaningful.


