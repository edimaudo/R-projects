#objective
#-Summarise and visualise the data.
#-Prepare the dataset for analysis (data cleansing, choose appropriate features to model)
#-Build model to predict final result

#data dictionary
# code_module – an identification code for a module on which the student is registered.
# code_presentation - the identification code of the presentation during which the student is registered on the module.
# id_student – a unique identification number for the student.
# gender – the student’s gender.
# region – identifies the geographic region, where the student lived while taking the module-presentation.
# highest_education – highest student education level on entry to the module presentation.
# imd_band – specifies the Index of Multiple Depravation band of the place where the student lived during the module-presentation.
# age_band – band of the student’s age.
# num_of_prev_attempts – the number times the student has attempted this module.
# studied_credits – the total number of credits for the modules the student is currently studying.
# disability – indicates whether the student has declared a disability.
# final_result – student’s final result in the module-presentation.

# remove old data
rm(list=ls())

#