### What is this repository? 
This repository is related to Boruta package in R.
It's my own project: Boruta_plus

Now, I'm developing 3 things below;  
(Product1) Boruta + Random forest vs Random forest alone  
(Product2) Boruta + Logistic regression vs Logistic regression alone  
(Product3) Boruta + Multiple regression vs Multiple regression alone  

---

### Boruta packages
Boruta is a so powerful package to improve and simplify dataset, as it's a novel feature selection algorithm.   
See: Miron B Kursa and Witold R. Rundnicki, [Feature Selection with the Boruta Package](https://www.jstatsoft.org/article/view/v036i11), Journal of Statistical Software, 2010.   


---

### folder: sample1_adult
data: [Adult](http://archive.ics.uci.edu/ml/datasets/Adult)  
aim : to determine whether a person makes over 50K(salary) a year.  
mthd: (1) Boruta + Random forest vs (2) Random forest alone  
rslt: no difference between (1) and (2). -> all attributes are meaningful.  


