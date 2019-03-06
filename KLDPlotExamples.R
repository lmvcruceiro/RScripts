plot(density(creditData$loan_amnt), ylim = c(0, 0.00008))
lines(density(creditData$ID))

plot(density(scale(creditData$loan_amnt[!is.na(creditData$loan_amnt)])))
lines(density(scale(creditData$total_pymnt[!is.na(creditData$total_pymnt)])))

# ac_client_district
# threshold = 0.3
plot(density(scale(ac_client_district$account_id[!is.na(ac_client_district$account_id)])), ylim = c(0, 1))
lines(density(scale(ac_client_district$trans_sum[!is.na(ac_client_district$trans_sum)])))

#bajar umbral
df <- fread("C:/master/MÓDULO III - Data Science/Practica_EVA_SEL_REG/ozonoChinaData.csv")
plot(density(scale(df$O3[!is.na(df$O3)])), ylim = c(0, 1))
lines(density(scale(df$O3_168[!is.na(df$O3_168)])))

#
df <- fread("C:/Users/lamendez/Downloads/energydata_complete.csv")
plot(density(scale(df$T8[!is.na(df$T8)])), ylim = c(0, 1))
lines(density(scale(df$RH_8[!is.na(df$RH_8)])))
