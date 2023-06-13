## code to prepare `breastcancer` dataset goes here

breastcancer <- tibble::tibble(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))
breastcancer$stage = factor(breastcancer$stage)
# Better alternative: labelled::set_variable_labels()
attr(x = breastcancer$death, which = "label") <- "Death"
attr(x = breastcancer$stage, which = "label") <- "Stage"
attr(x = breastcancer$receptor, which = "label") <- "Hormone receptor"

usethis::use_data(breastcancer, overwrite = TRUE)
