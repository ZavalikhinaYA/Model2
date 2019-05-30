library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")    

eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
eddypro = eddypro[-1, ]
eddypro = select(eddypro, -(roll))
eddypro = eddypro %>% mutate_if(is.character, factor)

names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "L_")

glimpse(eddypro)

eddypro = drop_na(eddypro)
eddypro = filter(eddypro, DOY >= 243 & DOY < 334)
eddypro = filter(eddypro, daytime==FALSE)
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = eddypro_numeric[teach,]
testing_tbl = eddypro_numeric[test,]

mod1 = lm(co2_flux~ (.) , data = teaching_tbl)

summary(mod1)
anova(mod1)
plot(mod1) 

mod2 = lm ( co2_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux
            + h2o_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density
            + h2o_mole_fraction + h2o_mixing_ratio + sonic_temperature + air_temperature
            + air_pressure + air_density + air_heat_capacity + air_molar_volume + e + es 
            + specific_humidity + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot
            + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + L + L_z_minus_dL__div_L
            + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_70_perc_ + un_Tau 
            + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + w_spikes + co2_spikes 
            + h2o_spikes + u_var + v_var + co2_var + h2o_var + w_div_h2o_cov + co2 + h2o 
            + co2_signal_strength_7200 + flowrate, data = teaching_tbl)

summary(mod2)
anova(mod2)
anova(mod2, mod1)
plot(mod2) 


mod3 = lm ( co2_flux~ DOY + file_records + Tau + qc_Tau + rand_err_Tau + H
            + qc_H, rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux
            + rand_err_co2_flux + h2o_flux + rand_err_h2o_flux+ H_strg
            + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density
            + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density + data = teaching_tbl)

summary(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

cor_teaching_tbl = select(teaching_tbl, DOY, file_records, Tau, qc_Tau, rand_err_Tau, H,
                          qc_H, rand_err_H, LE, qc_LE, rand_err_LE, qc_co2_flux,
                          rand_err_co2_flux, h2o_flux, rand_err_h2o_flux, H_strg,
                          co2_v_minus_adv, h2o_v_minus_adv, co2_molar_density,
                          co2_mole_fraction, co2_mixing_ratio, h2o_molar_density)
            
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
