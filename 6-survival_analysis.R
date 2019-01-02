# 1
library('survival')

gr1 = c(143, 165, 188, 188, 190, 192, 206, 208, 212, 216,220, 227, 230, 235, 246, 265, 303, 216, 244)
gr2 = c(142, 157, 163, 198, 205, 232, 232, 232, 233, 233, 233, 233, 239, 240, 261, 280, 280, 295, 295, 323, 204, 344)

(o.g1 <- Surv(gr1, c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0)))
(model1.c <- survfit(o.g1 ~ 1))
summary(model1.c)
print(model1.c, rmean = 'common')

(o.g2 <- Surv(gr2, c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0)))
(model2.c <- survfit(o.g2 ~ 1))
summary(model2.c)
print(model2.c, rmean = 'common')

library('survminer')

dane = data.frame(czasy = Surv(c(gr1, gr2), c(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0),c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0))),
                  grupa=c(rep(1,19),rep(2,22)))
fit <- survfit(czasy~grupa, data=dane)
ggsurvplot(fit, 
           data = dane,
           risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE, 
           fun = 'pct', 
           legend.labs = c('Group 1','Group 2'),
           legend.title = 'Rats')


# 2
library('survival')

cancer = data.frame(cancer)
View(cancer)

cancer = cancer[complete.cases(cancer),]
fit <- survfit(Surv(cancer$time, cancer$status) ~ sex, data = cancer)

ggsurvplot(fit, risk.table = TRUE, pval = TRUE, conf.int = TRUE, fun = 'pct', legend.labs = c('Female', 'Male'), legend.title = 'Sex')

print(fit, rmean = 'common')

survdiff(Surv(cancer$time, cancer$status) ~ sex, data = cancer)

fit.cox <- coxph(Surv(cancer$time, cancer$status) ~ sex + age + meal.cal, data = cancer)
summary(fit.cox)

step(fit.cox)

fit.cox1 <- coxph(Surv(cancer$time, cancer$status) ~ sex + age, data = cancer)
summary(fit.cox1)