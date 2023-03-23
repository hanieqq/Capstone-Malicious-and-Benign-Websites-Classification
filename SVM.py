#!/usr/bin/env python
# coding: utf-8
#Created by:
#Stephanie Wong 
#U3209816
#For the University of Canberra Capstone Project Semester 1, 2023.
#This code contains the set up, feature selection, train/test split,
#hyper parameter tuning, model set up, and model evaluation for the 
#SVM classifier on the Malicious and Benign Websites dataset.
#Format and code is loosely inspired from code I had created for a similar 
#project in the unit "Pattern Recognition and Machine Learning".


# In[1]: Package set up

#import packages
import pandas as pd
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import f_classif
from numpy import set_printoptions
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import cross_validate
import numpy as np
from sklearn import metrics
import matplotlib.pyplot as plt


# In[2]: Dataset set up


#load dataset
urls = pd.read_csv(
        "D:/University/ITS Capstone Project/Coding/websitesCleaned.csv")

#Extract feature columns
feature_columns = ['URL_LENGTH', 'NUMBER_SPECIAL_CHARACTERS', 'CHARSET', 
                           'SERVER', 'CONTENT_LENGTH', 'WHOIS_COUNTRY', 
                           'WHOIS_STATEPRO', 'WHOIS_REGDATE', 
                           'WHOIS_UPDATED_DATE', 'TCP_CONVERSATION_EXCHANGE', 
                           'DIST_REMOTE_TCP_PORT', 'REMOTE_IPS', 'APP_BYTES',
                           'SOURCE_APP_PACKETS', 'REMOTE_APP_PACKETS', 
                           'SOURCE_APP_BYTES', 'REMOTE_APP_BYTES',
                           'APP_PACKETS', 'DNS_QUERY_TIMES']
X = urls[feature_columns]
#extract target
y = urls.Type

# In[3]: Feature extraction

test = SelectKBest(score_func=f_classif, k=5)
fit = test.fit(X, y)
#summarize scores
set_printoptions(precision=3)

###displaying feature selection results
#fit feature selection
features = fit.transform(X)
scores = test.scores_

#print the different scores
sadge = 0
col = X.columns
print('Feature selection results')
while(sadge < 19):
    print(col[sadge])
    print(scores[sadge])
    sadge = sadge+1

#create a dataset with the 6 best scores + target
feature_columns = ['URL_LENGTH', 'NUMBER_SPECIAL_CHARACTERS',
                            'WHOIS_UPDATED_DATE', 'DIST_REMOTE_TCP_PORT', 
                            'DATE_DIFF', 'WHOIS_REGDATE']

#mark the target using a separate variable
X = urls[feature_columns]

# In[4]: Test/Train split


#Import train_test_split function
from sklearn.model_selection import train_test_split

#Split dataset into a training set and testing set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3,
                                                    random_state=0)


# In[5]: Hyper parameter tuning

#import the model
from sklearn.svm import SVC

# ##This code is commented out for ease of processing. 
# ##It was run for test sizes 0.4, 0.3, and 0.2.
# #import grid search
# from sklearn.model_selection import GridSearchCV

# #define parameters to try for SVM
# params = {'kernel': ('linear', 'poly', 'rbf', 'sigmoid'),
#           'C': (1,5,10,15,20),
#           'degree': (3,4,5,6,7),
#           'gamma': ('scale', 'auto'),
#           'class_weight': ('balanced', None),
#           'break_ties': ('True', 'False')
#           }


# #using 10 fold cross validation, fit each parameter combo on the model
# grs = GridSearchCV(SVC(), param_grid=params, cv = 10, n_jobs = -1)
# grs.fit(X_train, y_train)

# #display best parameter combo for the model
# y_pred=grs.predict(X_test)
# print("Best Hyper Parameters:",grs.best_params_)

# In[6]: Model parameters
##please note: initial results were discovered with model = SVC()
##this ensured that the initial comparison between models were fair, 
##as NB cannot use gridSearchCV

##define model with HPT (test size = 0.3)
##Best Hyper Parameters: {'C': 20, 'break_ties': 'True', 'class_weight': None, 
##'degree': 3, 'gamma': 'scale', 'kernel': 'rbf'}
model = SVC(C = 20, break_ties = 'True', class_weight = None, degree = 3, 
            gamma = 'scale', kernel = 'rbf')

#fit the model with training data
model.fit(X_train,y_train)

#make a prediction for test data
y_pred = model.predict(X_test)

# In[7]: Evaluation

#classification report -> precision
report = classification_report(y_test, y_pred, output_dict = True)
m_recall = report['1']['recall']

#model accuracy
score = model.score(X_test, y_test)
print('Accuracy', score, sep = '\n')


#confusion matrix
plt.rcParams.update({'font.size': 15})
print('Confusion Matrix')
cm = confusion_matrix(y_test,y_pred)
print(cm)
cm_display = metrics.ConfusionMatrixDisplay(
    confusion_matrix = cm, 
    display_labels = ['Benign', 'Malicious']
    )

cm_display.plot(
cmap = 'Purples')
plt.show()

#cross validation
print('Cross validation')
cv_results = cross_validate(model, X, y, cv=10)
sorted(cv_results.keys())
cv_variance = np.var(cv_results['test_score'])
print(cv_results['test_score'])
print('Cross validation variance', cv_variance, sep = '\n')

#AUC
fpr, tpr, _ = metrics.roc_curve(y_test,  y_pred)
auc = metrics.roc_auc_score(y_test, y_pred)
print("AUC", auc, sep = '\n')

#overall score
overall_score = (m_recall + 1 - (cv_variance * 1000) + auc) / 3
print('Overall score', overall_score, sep = '\n')