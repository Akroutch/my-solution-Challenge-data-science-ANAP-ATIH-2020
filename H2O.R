library(h2o)
h2o.init(nthreads=-1,max_mem_size = "6G")

train.h2o <- as.h2o(trainX)
test.h2o <- as.h2o(testX)

colnames(train.h2o)


#dependent variable (Purchase)
y.dep <- 9

#independent variables (dropping ID variables)
x.indep <- c(6,8,10,13,14,16,17,24)



####################### Multiple Regression in H2O#####

regression.model <- h2o.glm( y = y.dep, x = x.indep, 
                             training_frame = train.h2o, family = "gaussian")

h2o.performance(regression.model)

#writing submission file
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))
cible = predict.reg$predict # Predit le prix des boites de medicaments de l'echantillon de test
soumission = data.frame(id = id, cible=cible) # Cree un data.frame au bon format pour la soumission
soumission[soumission$cible < 0 , ] = 0
write.table(soumission, file = 'soumissionGLM1.csv', sep = ';', row.names = F) # Sauvegarde la soumission



####################Random Forest in H2O#####

#Random Forest
system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o
                                    ,ntrees = 3000
                                    , mtries = 7, max_depth = 8, seed = 1122
                                    )
)

h2o.performance(rforest.model)

#check variable importance
sss=h2o.varimp(rforest.model)
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))

#writing submission file
pred = predict.rforest$predict # Predit le prix des boites de medicaments de l'echantillon de test
pred2 = data.frame(id = id, cible=pred) # Cree un data.frame au bon format pour la soumission

write.table(pred2, file = 'randomForest.csv', sep = ';', row.names = F) # Sauvegarde la soumission




################GBM
system.time(
  gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o
                       ,ntrees = 2000, max_depth = 7, learn_rate = 0.1, seed = 1122
                       )
)

h2o.performance (gbm.model)
ss2=h2o.varimp(gbm.model)

#making prediction and writing submission file
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))


#writing submission file

cible = predict.gbm$predict # Predit le prix des boites de medicaments de l'echantillon de test
soumission = data.frame(id = id, cible = cible) # Cree un data.frame au bon format pour la soumission

soumission[soumission$predict < 0 , ] = 0
soumission[soumission$predict < 0.000198962 & soumission$predict > 0] = 0.000198962

write.table(soumission, file = 'soumissionGBM9.csv', sep = ';', row.names = F) # Sauvegarde la soumission



##########################  deep learning models

system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o
                                      #,epoch = 20,
                                      #hidden = c(600,600,600),
                                      #l1=1e-5,
                                      #seed = 1122
  )
)

h2o.performance(dlearning.model)


#making predictions
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))

#create a data frame and writing submission file
#writing submission file
prix = exp(predict.dl2$predict) # Predit le prix des boites de medicaments de l'echantillon de test
soumission = data.frame(id = test$id, prix) # Cree un data.frame au bon format pour la soumission
write.table(soumission, file = 'soumission_dl_ENS.csv', sep = ';', row.names = F) # Sauvegarde la soumission





















################GBM
system.time(
  gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o)
)

h2o.performance (gbm.model)
ss2=h2o.varimp(gbm.model)
