view(refine_original)
library(tidyr)
library(dplyr)

#1: Clean up brand names(to lower case)
refine_original <- transform(refine_original, company =tolower(company))
"philips"-> refine_original$company[refine_original$company =="phillips"]
"philips"-> refine_original$company[refine_original$company == "phllips"]
"philips"-> refine_original$company[refine_original$company == "phlips"]
"philips"-> refine_original$company[refine_original$company == "phillps"]
"philips"-> refine_original$company[refine_original$company == "fillips"]
"akzo" -> refine_original$company[refine_original$company == "akz0"]
"akzo" -> refine_original$company[refine_original$company == "ak zo"]
"akzo" -> refine_original$company[refine_original$company == "azko"]
"unilever" -> refine_original$company[refine_original$company == "unilver"]

#2: Separate product code and number
refine_original <- separate(refine_original,Product.code...number,c("product_code","product_number"),sep="-")

#3: Add product categories 
refine_original <-mutate(refine_original,"product_category" 
=ifelse(product_code =="p","Smartphone", ""))%>%
mutate("product_category" = ifelse(product_code == "v","TV",product_category))%>%
mutate("product_category" = ifelse(product_code == "x", "Laptop",product_category))%>%
mutate("product_category" = ifelse(product_code =="q","Tablet", product_category))

#4: Add full address for geocoding
refine_original<-unite(refine_original,"full_address", address,city,country, sep = ",")

#5:Create dummy variables for company and product category
refine_original<-mutate(refine_original,"company_philips"= ifelse(company=="philips",1,0))%>%
  mutate("company_akzo" = ifelse(company=="akzo",1,0))%>%
  mutate("company_van houten" = ifelse(company=="van houten",1,0))%>%
  mutate("company_unilever" = ifelse(company=="unilever",1,0))

refine_original<-mutate(refine_original,"product_smartphone" = ifelse(product_category=="Smartphones",1,0))%>%
  mutate("product_tv" = ifelse(product_category=="TV",1,0))%>%
  mutate("product_laptop" = ifelse(product_category=="Laptop",1,0))%>%
  mutate("product_tablet" = ifelse(product_category=="Tablet",1,0))

write.csv(refine_original,"refine_clean.csv")




