#### LIBRARIES #### 
library(cowplot)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(waffle)
library(RColorBrewer)
library(extrafont)
library(tidyverse)
library(scales)

#### MODEL EQUATIONS #### 
# to allow for time-varying detection,
# we implement these as difference equations
run_model = function(t, N,
                     S, E, I_m, R_mu, R_md, I_s, H_su, H_sd, R_su, R_sd,
                     I_c, H_cu, H_cd, C_cu, C_cd, R_cu, R_cd,
                     lambda, v, f_c, f_s, gamma, alpha,
                     p_m, p_s, p_c, delta_s, delta_c, epsilon_c){
  
  # initialize compartments
  S = c(S, rep(0, t-1))
  E = c(E, rep(0, t-1))
  I_m = c(I_m, rep(0, t-1))
  R_mu = c(R_mu, rep(0, t-1))
  R_md = c(R_md, rep(0, t-1))
  I_s = c(I_s, rep(0, t-1))
  H_su = c(H_su, rep(0, t-1))
  H_sd = c(H_sd, rep(0, t-1))
  R_su = c(R_su, rep(0, t-1))
  R_sd = c(R_sd, rep(0, t-1))
  I_c = c(I_c, rep(0, t-1))
  H_cu = c(H_cu, rep(0, t-1))
  H_cd = c(H_cd, rep(0, t-1))
  C_cu = c(C_cu, rep(0, t-1))
  C_cd = c(C_cd, rep(0, t-1))
  R_cu = c(R_cu, rep(0, t-1))
  R_cd = c(R_cd, rep(0, t-1))
  
  # cumulatives
  I_cum = c(I_m[1] + I_c[1] + I_s[1], rep(0, t-1))
  I_cum_det = c(R_md[1] + H_sd[1] + H_cd[1], rep(0, (t-1)))
  H_cum = c(H_cu[1] + H_cd[1] + H_su[1] + H_sd[1], rep(0, t-1))
  H_cum_det = c(H_cd[1] + H_sd[1], rep(0, t-1))
  ICU_cum = c(H_cd[1] + H_sd[1], rep(0, t-1))
  
  # fraction mild
  f_m = 1 - f_s - f_c

  for(i in 1:(t-1)){
    
    # time changing variables
    I_star = (I_m[i]*alpha + I_s[i] + I_c[i])

    # susceptible + exposed
    S[i+1] = S[i] -lambda[i]*I_star/N*S[i]
    E[i+1] =  E[i] + lambda[i]*I_star/N*S[i] - v*E[i]
    
    # mild
    I_m[i+1] = I_m[i] + v*E[i]*f_m - gamma*I_m[i]
    R_mu[i+1] = R_mu[i] + gamma*I_m[i]*(1-p_m[i])
    R_md[i+1] = R_md[i] + gamma*I_m[i]*p_m[i]
    
    # severe
    I_s[i+1] = I_s[i] + v*E[i]*f_s - gamma*I_s[i]
    H_su[i+1] = H_su[i] + gamma*I_s[i]*(1-p_s) - delta_s*H_su[i]
    H_sd[i+1] = H_sd[i] + gamma*I_s[i]*p_s - delta_s*H_sd[i]
    R_su[i+1] = R_su[i] + delta_s*H_su[i]
    R_sd[i+1] = R_sd[i] + delta_s*H_sd[i]
    
    
    # critical
    I_c[i+1] = I_c[i] + v*E[i]*f_c - gamma*I_c[i]
    H_cu[i+1] = H_cu[i] + gamma*I_c[i]*(1-p_c) - delta_c*H_cu[i]
    H_cd[i+1] = H_cd[i] + gamma*I_c[i]*p_c - delta_c*H_cd[i]
    C_cu[i+1] = C_cu[i] + delta_c*H_cu[i] - epsilon_c*C_cu[i]
    C_cd[i+1] = C_cd[i] + delta_c*H_cd[i] - epsilon_c*C_cd[i]
    R_cu[i+1] = R_cu[i] + epsilon_c*C_cu[i]
    R_cd[i+1] = R_cd[i] + epsilon_c*C_cd[i]
    
    # cumulatives
    I_cum[i+1] = I_cum[i] + v*E[i]
    I_cum_det[i+1] = I_cum_det[i] +  gamma*I_m[i]*p_m[i] + gamma*I_s[i]*p_s + gamma*I_c[i]*p_c
    H_cum[i+1] = H_cum[i] + gamma*I_c[i] + gamma*I_s[i]
    H_cum_det[i+1] = H_cum_det[i]+ gamma*I_c[i]*p_c + gamma*I_s[i]*p_s
    ICU_cum[i+1] = ICU_cum[i] + delta_c*H_cu[i] + delta_c*H_cd[i]

  }
  
  return(data.frame(time = 1:t, S, E, I_m, R_mu, R_md, I_s, H_su, H_sd, R_su, R_sd,
                    I_c, H_cu, H_cd, C_cu, C_cd, R_cu, R_cd, I_cum, I_cum_det, H_cum, H_cum_det, ICU_cum))
  
}

#### TRANSLATE FROM APP TO MODEL ####
process_params = function(N = 1938000, E = 24, I_m =4, R_mu = 0, R_md = 4, 
                          I_s = 0, H_su = 0, H_sd = 0, R_su = 0, R_sd = 0,
                          I_c = 0, H_cu = 0, H_cd = 0, C_cu = 0, C_cd = 0, R_cu = 0, R_cd =0,
                          contact_mod_1 = .8, contact_mod_2 = .4, alpha = 1,
                          t = 50, 
                          lambda = .85,
                          pmbase = 0.05, pmint1 = 0.1, pmint2 = 0.15, v = 1/5, f_c = 0.01, f_s = 0.03, gamma = 1/5,
                          p_s = .9, p_c = .9, delta_s = 1/8, delta_c = 1/6, epsilon_c = 1/10){
  
  # set initial conditions
  # commented out version are heuristics that might be helpful
  # if you're not inputting all of them
  #I_m = R_md/gamma
  #E = I_m + R_md
  S = N-E-I_m-R_md  

  # set initial conditions
  p_m = c(seq(pmbase, pmint1, length.out = 17), seq(pmint1, pmint2, length.out = 15), rep(pmint2, t-32)) 

  # lambda with and without intervention
  lambda_int = c(rep(lambda, 17), rep(lambda*contact_mod_1, 15), rep(lambda*contact_mod_2, t-32))
  lambda = rep(lambda, t)

  # collapse these more nicely but they're fine for now
  # run the model with intervention
  out_int = run_model(t = t, N = N, S = S, E = E, I_m = I_m, R_mu = R_mu, R_md = R_md, 
                  I_s = I_s, H_su = H_su, H_sd = H_sd, R_su = R_su, R_sd = R_sd,
                  I_c = I_c, H_cu = H_cu, H_cd = H_cd, C_cu = C_cu, C_cd = C_cd, R_cu = R_cu, R_cd =R_cd,
                  lambda = lambda_int, v = v, f_c = f_c, f_s = f_s, gamma = gamma, alpha = alpha,
                  p_m = p_m, p_s = p_s, p_c = p_c, delta_s = delta_s, delta_c = delta_c, epsilon_c = epsilon_c) %>% mutate(int = "Social distancing")
  
  # run the model without intervention
  out_no_int = run_model(t = t, N = N, S = S, E = E, I_m = I_m, R_mu = R_mu, R_md = R_md, 
                         I_s = I_s, H_su = H_su, H_sd = H_sd, R_su = R_su, R_sd = R_sd,
                         I_c = I_c, H_cu = H_cu, H_cd = H_cd, C_cu = C_cu, C_cd = C_cd, R_cu = R_cu, R_cd =R_cd,
                      lambda = lambda, v = v, f_c = f_c, f_s = f_s, gamma = gamma, alpha = alpha,
                      p_m = p_m, p_s = p_s, p_c = p_c, delta_s = delta_s, delta_c = delta_c, epsilon_c = epsilon_c) %>% mutate(int = "No intervention")
  
  return(bind_rows(out_int, out_no_int) %>% mutate(H = H_sd + H_su + H_cu + H_cd,
                                                   ICU = C_cu + C_cd))
}

# make plots
make_plots = function(out) {
  
  # Make data frames
  a.data = out %>% gather(var, value, I_cum, I_cum_det) %>% group_by(var, int) %>% 
    mutate(value_diff = c(NA, diff(value))) %>%
    gather(var2, value2, value, value_diff) %>%
    mutate(var_lab = ifelse(var == "I_cum", "All", "Detected"),
           cat_lab = ifelse(var2=="value", "Cumulative cases", "New cases"))
  
  b.data = out %>% gather(var, value, H, H_cum, ICU, ICU_cum) %>% 
      mutate(loc_lab = ifelse(grepl("ICU", var), "ICU", "All"),
             cum_lab = ifelse(grepl("cum", var), "Cumulative hospitalizations", "Current hospitalizations"))
  
  # Intervention
  a = ggplot(a.data %>% filter(int == "Social distancing"), 
         aes(x = time, y = value2, group = var_lab, col = var_lab)) + geom_line(lwd = 1.5) + 
    theme_minimal(base_size = 25) + facet_wrap(.~cat_lab, scales = "free") + labs(x = "", y = "") + scale_color_discrete(name = "") + 
    scale_y_continuous(label = comma)
  
  b = ggplot(b.data %>% filter(int == "Social distancing"), 
             aes(x = time, y = value, group = loc_lab, col = loc_lab)) + geom_line(lwd = 1.5) + 
    theme_minimal(base_size = 25) + facet_wrap(.~cum_lab, scales = "free") + labs(x = "", y = "") + scale_color_discrete(name = "") + 
    scale_y_continuous(label = comma)
  

  # No intervention
  c = ggplot(a.data %>% filter(int == "No intervention"), 
             aes(x = time, y = value2, group = var_lab, col = var_lab)) + geom_line(lwd = 2) + 
    theme_minimal(base_size = 25) + facet_wrap(.~cat_lab, scales = "free") + labs(x = "", y = "") + scale_color_discrete(name = "") + 
    scale_y_continuous(label = comma)
  
  d = ggplot(b.data %>% filter(int == "No intervention"), 
             aes(x = time, y = value, group = loc_lab, col = loc_lab)) + geom_line(lwd = 2) + 
    theme_minimal(base_size = 25) + facet_wrap(.~cum_lab, scales = "free") + labs(x = "", y = "") + scale_color_discrete(name = "") + 
    scale_y_continuous(label = comma)
  
  return(list(a, b, c, d))
  
  
  
}

# calibration plots
calib_plots = function(out, data){
  
  # read in and label datat
  # bind with data frame
  # label according to relevant time periods
  ts = data %>% gather(var, value, cum_cases, hosp_ongoing, hosp_new) %>% mutate(id = "Observed") %>% 
    bind_rows(out %>% filter(int == "Social distancing") %>% mutate(hosp_new = c(NA, diff(H_cum))) %>% 
                rename(cum_cases = I_cum_det, hosp_ongoing = H) %>% 
                select(time, cum_cases, hosp_ongoing, hosp_new) %>% mutate(id = "Predicted") %>%
                gather(var, value, cum_cases, hosp_ongoing, hosp_new)) %>%
    mutate(split = ifelse(time >= 18, "March 17-March 31", "Feb 29-March 16"),
           split = ifelse(time >= 33, "April 1 onward", split),
           split = factor(split, levels = c("Feb 29-March 16", "March 17-March 31", "April 1 onward"))) %>% 
    mutate(var2 = ifelse(var == "cum_cases", "Cumulative detected cases", "Current hospitalizations"),
           var2 = ifelse(var=="hosp_new", "New admissions", var2))
  
  # make plot
  a = ggplot(ts, aes(x = time, y = value, group = id, col = id)) + geom_point() + geom_line() + 
    labs(x = "", y = "") + scale_color_discrete(name = "") +  scale_y_continuous(label = comma) + theme_minimal(base_size = 25) + 
    facet_wrap(var2~split, scales = "free")
  
  return(a)
  
}
