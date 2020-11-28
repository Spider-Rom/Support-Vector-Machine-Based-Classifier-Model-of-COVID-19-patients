load("SVM model.RData")

require(e1071)

clinical_data = read.table("Clinical data (example).txt", #change the file name if necessary
                            sep = "\t", stringsAsFactors = FALSE, header = TRUE)

colnames(clinical_data) = c("Patient id","AST","Alb","LDH","Lymph_rate","Lymph_count","Neu_count","Eos_rate","Eos_count","Baso_count","D_Dimer","PTA","CRP","PT_INR","fever","aversion.to.cold.or.shivering","cough","nasal.obstruction.or.nasal.discharge","pharyngodynia","dyspnea","diarrhea","abdominal.pain","nausea","vomiting","inappetence","hypodynamia","headache","muscle.soreness","palpitations","night.sweat","dizziness")

for (i in 2:14) {
  clinical_data[, i] = as.numeric(clinical_data[, i])
}

for (i in 15:31) {
  clinical_data[, i] = factor(as.character(clinical_data[, i]), levels = as.character(c("No", "Yes")))
}

prediction_result = predict(svm_model, clinical_data, probability = FALSE)

prediction_result_with_probablities = predict(svm_model, clinical_data, probability = TRUE)

prediction_result_dataframe = data.frame(`Patient id` = clinical_data$`Patient id`,
                                         Result = gsub("3", "Cluster C", gsub("2", "Cluster B", gsub("1","Cluster A", prediction_result, fixed = TRUE), fixed = TRUE), fixed = TRUE),
                                         `Probability of Cluster A` = round(attributes(prediction_result_with_probablities)$probabilities[,1],digits = 5),
                                         `Probability of Cluster B` = round(attributes(prediction_result_with_probablities)$probabilities[,2],digits = 5),
                                         `Probability of Cluster C` = round(attributes(prediction_result_with_probablities)$probabilities[,3],digits = 5))


prediction_result_dataframe

