library("haven")
library("rigr")
library("tidyverse")
library("ggplot2")
library("gridExtra")

#load in data ------------------------------------------------------------------
insom <- read_sav("InsomniaData.sav")
View(insom)

#create new score and category variables ---------------------------------------
insom$insom_cut <- insom$Total_Insomnia
insom$insom_cut[insom$Total_Insomnia >= 19] <- 1
insom$insom_cut[insom$Total_Insomnia < 19] <- 0

insom$paranoia_cut <- insom$Total_Paranoia
insom$paranoia_cut[insom$Total_Paranoia >= 34] <- 1
insom$paranoia_cut[insom$Total_Paranoia < 34] <- 0

insom$na_cut <- insom$Total_Negative_Affect
insom$na_cut[insom$Total_Negative_Affect <= 21 ] <- "Low"
insom$na_cut[insom$Total_Negative_Affect > 21 & insom$Total_Negative_Affect <= 42] <- "Medium"
insom$na_cut[insom$Total_Negative_Affect > 42] <- "High"

insom$na_cut <- factor(insom$na_cut, levels = c("Low", "Medium", "High"))

insom$tst_cut <- insom$TST
insom$tst_cut[insom$TST >= 420] <- 0
insom$tst_cut[insom$TST < 420] <- 1 

#create subsetted data ---------------------------------------------------------
male_data <- subset(insom, Gender == 1)
fem_data <- subset(insom, Gender == 2)

age1 <- subset(insom, Age >= 17 & Age < 38)
age2 <- subset(insom, Age >= 38 & Age < 58)
age3 <- subset(insom, Age >= 58)

sleep_mon <- subset(insom, !is.na(TST))

#descriptive statistics --------------------------------------------------------
summary(insom$Age)
table(insom$Gender)
summary(insom$Total_Negative_Affect)
#ranges from 0 to 63
summary(insom$Total_Paranoia)
#ranges from 16 to 76

#figure 1
hist(insom$Total_Paranoia, 
     main = "Total Paranoia Scores for All Subjects", 
     xlab = "Paranoia Scores",
     breaks = 25)

summary(insom$Total_Insomnia)
#ranges from 8 to 32

#figure 2
hist(insom$Total_Insomnia, 
     main = "Total Insomnia Scores for All Subjects",
     xlab = "Insomnia Scores",
     breaks = 25)

#descriptive statistics tables--------------------------------------------------
tab <- insom %>% 
  dplyr::select(Total_Paranoia, Total_Insomnia, Total_Negative_Affect) %>%
  descrip() %>%
  signif(3)

#table 1: descriptive stats for scores
tab <- tab[, 3:4] %>% as.data.frame()
tab <- t(tab)
rownames(tab) <- c("Mean", "SD")
colnames(tab) <- c("Paranoia (range from 16-76)", 
                   "Insomnia (range from 8-32)", 
                   "Negative affectivity (range from 0-63)")
knitr::kable(tab, format = "markdown",
             caption = "Descriptive statistics for total paranoia, insomnia, and negative affectivity scores")

#table 2: descriptive stats for proportions of those w/ and w/o each category
tab2 <- data.frame(prop = c(mean(insom$paranoia_cut), 
                          mean(insom$insom_cut), 
                          table(insom$na_cut)[1]/nrow(insom), 
                          table(insom$na_cut)[2]/nrow(insom), 
                          table(insom$na_cut)[3]/nrow(insom))) %>%
                              round(2)
tab2 <- t(tab2)
rownames(tab2) <- c("Proportion")
colnames(tab2) <- c("Paranoia", "Insomnia", "Low NA", "Medium NA", "High NA")

knitr::kable(tab2, format = "markdown",
             caption = "Proportions of individuals with paranoia, insomnia, and negative affectivity (by category)")
mean(age1$paranoia_cut)

#table 3: descriptive stats for proportions of those w/ and w/o each category dependent on age group
tab3 <- data.frame(para = c(mean(age1$paranoia_cut), 
                            mean(age2$paranoia_cut), 
                            mean(age3$paranoia_cut)), 
                   ins = c(mean(age1$insom_cut), 
                           mean(age2$insom_cut), 
                           mean(age3$insom_cut)), 
                   na_low = c(table(age1$na_cut)[1]/nrow(age1), 
                             table(age2$na_cut)[1]/nrow(age2), 
                             table(age3$na_cut)[1]/nrow(age3)),
                   na_med = c(table(age1$na_cut)[2]/nrow(age1), 
                             table(age2$na_cut)[2]/nrow(age2), 
                             table(age3$na_cut)[2]/nrow(age3)), 
                   na_high = c(table(age1$na_cut)[3]/nrow(age1), 
                              table(age2$na_cut)[3]/nrow(age2), 
                              table(age3$na_cut)[3]/nrow(age3))) %>%
                      round(2)
tab3 <- t(tab3)
rownames(tab3) <- c("Paranoia", "Insomnia", "Low NA", "Medium NA", "High NA")
colnames(tab3) <- c("Age 18-37 (n=266)", "Age 38-57 (n=146)", "Age 58-77 (n=27)")

knitr::kable(tab3, format = "markdown",
             caption = "Proportions of individuals with paranoia, insomnia, and negative affectivity across three age groups")

#table 4: descriptive stats for proportions of those w/ and w/o each category dependent on sex
tab4 <- data.frame(para = c(mean(fem_data$paranoia_cut),
                            mean(male_data$paranoia_cut)), 
                   ins = c(mean(fem_data$insom_cut), 
                           mean(male_data$insom_cut)), 
                   na_low = c(table(fem_data$na_cut)[1]/nrow(fem_data),
                             table(male_data$na_cut)[1]/nrow(male_data)),
                   na_med = c(table(fem_data$na_cut)[2]/nrow(fem_data), 
                             table(male_data$na_cut)[2]/nrow(male_data)), 
                   na_high = c(table(fem_data$na_cut)[3]/nrow(fem_data),
                              table(male_data$na_cut)[3]/nrow(male_data))) %>%
  round(2)

tab4 <- t(tab4)
rownames(tab4) <- c("Paranoia", "Insomnia", "Low NA", "Medium NA", "High NA")
colnames(tab4) <- c("Female (n=336)", "Male (n=103)")

knitr::kable(tab4, format = "markdown",
             caption = "Proportions of individuals with paranoia, insomnia, and negative affectivity across sex subgroups")


#figure 3: scatterplot of total paranoia given total insomnia
pal = c("purple", "orange", "red")
insom %>%
  ggplot(aes(x = Total_Insomnia, y = Total_Paranoia, col = NA_Cut)) +
  geom_point(alpha = 0.35) +
  geom_smooth(aes(col = NA_Cut), se = FALSE, method = "loess") +
  labs(x="Total Insomnia Score", y = "Total Paranoia Score", col = "Negative Affectivity") +
  scale_color_manual(values = pal)


#primary analysis --------------------------------------------------------------

prim_mod <- regress("odds", paranoia_cut~insom_cut+na_cut, data = insom)
simple_mod <- regress("odds", paranoia_cut~insom_cut, data = insom)

#table 5: primary model coefficients
tab5 <- data.frame(intercept = c(round(coef(prim_mod)[1,4],3),
                               "(0.005, 0.037)",
                               "<0.001"), 
                   ins = c(round(coef(prim_mod)[2,4],2),
                         "(0.19, 2.05)",
                         round(coef(prim_mod)[2,8],2)), 
                   na_med = c(round(coef(prim_mod)[3,4],2), 
                            "(4.01, 54.68)",
                            "<0.001"),
                   na_high = c(round(coef(prim_mod)[4,4],2),
                             "(10.15, 335.2)",
                             "<0.001"))
tab5 <- t(tab5)
rownames(tab5) <- c("Intercept", "Insomnia level", "Medium NA", "High NA")
colnames(tab5) <- c("Estimate", "95% CI", "P-value")

knitr::kable(tab5, format = "markdown",
             caption = "Multiple logistic regression model coefficient estimates")

#table 6: simple model coefficients
tab6 <- data.frame(intercept = c(round(coef(simple_mod)[1,4],3), 
                               "(0.013, 0.06)",
                               "<0.001"), 
                   ins = c(round(coef(simple_mod)[2,4],2),
                         "(0.76, 5.48)",
                         round(coef(simple_mod)[2,8],2))) 
tab6 <- t(tab6)
rownames(tab6) <- c("Intercept", "Insomnia level")
colnames(tab6) <- c("Estimate", "95% CI", "P-value")

knitr::kable(tab6, format = "markdown",
             caption = "Simple logistic regression model coefficient estimates")

#secondary analysis ------------------------------------------------------------
mal_mod <- regress("odds", paranoia_cut~insom_cut+na_cut, data = male_data)
fem_mod <- regress("odds", paranoia_cut~insom_cut+na_cut, data = fem_data)
sleep_mon_mod <- regress("odds", paranoia_cut~tst_cut+na_cut, data = sleep_mon)
age1_mod <- regress("odds", paranoia_cut~insom_cut+na_cut, data = age1)
age2_mod <- regress("odds", paranoia_cut~insom_cut+na_cut, data = age2)
age3_mod <- regress("odds", paranoia_cut~insom_cut+na_cut, data = age3)

#simple logistic regression by individual insomnia symptom
i1_mod <- regress("odds", paranoia_cut~i1, data = insom)
i2_mod <- regress("odds", paranoia_cut~i2, data = insom)
i3_mod <- regress("odds", paranoia_cut~i3, data = insom)
i4_mod <- regress("odds", paranoia_cut~i4, data = insom)
i5_mod <- regress("odds", paranoia_cut~i5, data = insom)
i6_mod <- regress("odds", paranoia_cut~i6, data = insom)
i7_mod <- regress("odds", paranoia_cut~i7, data = insom)
i8_mod <- regress("odds", paranoia_cut~i8, data = insom)

#figure 4: OR figure
i_symptoms <- data.frame(
  label = c("i1", "i2", "i3", "i4", "i5", "i6", "i7", "i8"),
  OR = c(coef(i1_mod)[2,4],
         coef(i2_mod)[2,4],
         coef(i3_mod)[2,4],
         coef(i4_mod)[2,4],
         coef(i5_mod)[2,4],
         coef(i6_mod)[2,4],
         coef(i7_mod)[2,4],
         coef(i8_mod)[2,4]),
  LL = c(coef(i1_mod)[2,5],
         coef(i2_mod)[2,5],
         coef(i3_mod)[2,5],
         coef(i4_mod)[2,5],
         coef(i5_mod)[2,5],
         coef(i6_mod)[2,5],
         coef(i7_mod)[2,5],
         coef(i8_mod)[2,5]),
  UL = c(coef(i1_mod)[2,6],
         coef(i2_mod)[2,6],
         coef(i3_mod)[2,6],
         coef(i4_mod)[2,6],
         coef(i5_mod)[2,6],
         coef(i6_mod)[2,6],
         coef(i7_mod)[2,6],
         coef(i8_mod)[2,6]),
  P = c(coef(i1_mod)[2,8],
        coef(i2_mod)[2,8],
        coef(i3_mod)[2,8],
        coef(i4_mod)[2,8],
        coef(i5_mod)[2,8],
        coef(i6_mod)[2,8],
        coef(i7_mod)[2,8],
        coef(i8_mod)[2,8])
)

plot1 <- ggplot(i_symptoms, aes(x = OR, y = label)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, 
             color = "red", 
             linetype = "dashed", 
             cex = 1, 
             alpha = 0.5) +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() 

table_base <- ggplot(i_symptoms, aes(y = label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        axis.text.x = element_text(color = "white", 
                                   hjust = -3, 
                                   size = 25),
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())


## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = (label), 
                x = 1, 
                label = sprintf("%0.1f", round(OR, digits = 1))),
            size = 4) + 
  ggtitle("OR")

#p values
tab2 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = (label), 
                x = 1, 
                label = round(P, 4), 
                size = 4)) + 
  ggtitle("P-value")

lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)

grid.arrange(plot1, tab1, tab2, layout_matrix = lay)
