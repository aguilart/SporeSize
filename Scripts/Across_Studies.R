
AcrossStudiesTraits<-
  rbind(CommTraitsBonfin2013,
      CommTraitsOehl2011,
      CommTraitsSturmer2001,
      CommTraitsBrundrett2013)

AcrossStudiesTraits

library(lme4)

TraitModel1<-
lmer(CWMean~bray.dist+
       (1|Study),data = AcrossStudiesTraits)

library(car)

Anova(TraitModel1)
summary(TraitModel1)


visreg(TraitModel1)


