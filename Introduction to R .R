#Creating matrices
mat_A <- matrix(1:10,nrow=5)
mat_A
mat_B <- matrix(1:10,ncol=5)
mat_B
mat_C <- matrix(11:20,5)#rows
mat_C
mat_D <- matrix(11:20, ,5)#columns
mat_D

#Creating vectors
participant_name <- c("Anil","badri","Chetna","Dinesh","Elisa")
participant_income <- c(6000,10000,45000,NA,70000)
participant_sport <- c("hockey","cricket","basketball","hockey","basketball")
participant_qualification <- factor(c("UG","PhD","UG","PG","UG"),levels=c("UG","PG","PhD"),ordered=TRUE)

#Creating dataframe
df1_participant <- data.frame(participant_name,participant_qualification,participant_income,participant_sport)
df1_participant
