# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
# importing libraries

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# import dataset
dataset=pd.read_csv('Data_train.csv')
dataset= dataset.dropna() #dropping missing value rows as it is of no use here
dataset.drop(["Loan_ID"],axis=1, inplace= True) #dropping loan_id, no use

X= dataset.iloc[:, :-1].values

Y= dataset.iloc[:, 11].values



# Encodign categorical data
from sklearn.preprocessing import LabelEncoder
labelencoder_Y= LabelEncoder()
Y= labelencoder_Y.fit_transform(Y)

labelencoder_X= LabelEncoder()
X[:,0]= labelencoder_X.fit_transform(X[:,0])
X[:,1]= labelencoder_X.fit_transform(X[:,1])
X[:,3]= labelencoder_X.fit_transform(X[:,3])
X[:,4]= labelencoder_X.fit_transform(X[:,4])

from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer

# converting variable property area using hot key encoder 
ct = ColumnTransformer([('encoder', OneHotEncoder(), [10])], remainder='passthrough')

X = np.array(ct.fit_transform(X), dtype=np.float)

# Splitting of data in training and test set

from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.25, random_state = 0)


# feature scaling of variables

from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()

X_train = sc_X.fit_transform(X_train)

X_test = sc_X.transform(X_test)

###--------------------------------------------------------------------

#Building Models- Classification

# fitting Logistics Regression model on training set
from sklearn.linear_model import LogisticRegression
classifier1 = LogisticRegression (random_state= 0)
classifier1.fit(X_train, y_train)

# predicting test set results using classifier
y_pred = classifier1.predict(X_test)

# making confusion matrix
from sklearn.metrics import confusion_matrix
cm1 = confusion_matrix(y_test,y_pred) 

#----------------------------------

# fitting K-Nearest Neighbors model on training set
from sklearn.neighbors import KNeighborsClassifier
classifier2= KNeighborsClassifier(n_neighbors = 5, metric = 'minkowski', p = 2)
classifier2.fit(X_train, y_train)

# predicting test set results using classifier
y_pred = classifier2.predict(X_test)

# making confusion matrix
from sklearn.metrics import confusion_matrix
cm2 = confusion_matrix(y_test,y_pred) 

#----------------------------------

# fitting SVM model on training dataset

from sklearn.svm import SVC
classifier3= SVC (kernel ='linear', random_state =0)
classifier3.fit(X_train, y_train)

# predicting test set results using classifier
y_pred = classifier3.predict(X_test)

# making confusion matrix
from sklearn.metrics import confusion_matrix
cm3 = confusion_matrix(y_test,y_pred) 

# This seems to be the BEST MODEL fitting to the problem ********
#----------------------------------

#fitting naive bayes model on training data set

from sklearn.naive_bayes import GaussianNB
classifier4 = GaussianNB()
classifier4.fit(X_train, y_train)

# predicting test set results using classifier
y_pred = classifier4.predict(X_test)

# making confusion matrix
from sklearn.metrics import confusion_matrix
cm4 = confusion_matrix(y_test,y_pred) 

#----------------------------------

#fitting Decision Tree model on training data set

from sklearn.tree import DecisionTreeClassifier
classifier5 = DecisionTreeClassifier(criterion = 'entropy', random_state= 0)
classifier5.fit(X_train, y_train)

# predicting test set results using classifier
y_pred = classifier5.predict(X_test)

# making confusion matrix
from sklearn.metrics import confusion_matrix
cm5 = confusion_matrix(y_test,y_pred) 

#----------------------------------

#fitting Random Forest model on training data set

from sklearn.ensemble import RandomForestClassifier
classifier6 = RandomForestClassifier (n_estimators = 100, criterion = 'entropy', random_state = 0)
classifier6.fit(X_train, y_train) 


# predicting test set results using classifier
y_pred = classifier6.predict(X_test)

# making confusion matrix
from sklearn.metrics import confusion_matrix
cm6 = confusion_matrix(y_test,y_pred) 









####--------------*************--------------------########---------------
# Cleaning the submission data and applying the BEST MODEL- SVM and submitting the prediction



# applying same codes on GIVEN test data


# import dataset
dataset_pred=pd.read_csv('Data_test.csv')
dataset_pred= dataset_pred.dropna() #dropping missing value rows as it is of no use here


sub_file = dataset_pred.iloc[:,0]
sub_file =pd.DataFrame(sub_file) # seperating out the Loan id for later use with prediction values

dataset_pred.drop(["Loan_ID"],axis=1, inplace= True)  #dropping loan_id, no use
dataset_pred= dataset_pred.iloc[:, :].values # all rows and all columns

# Encodign categorical data
from sklearn.preprocessing import LabelEncoder


labelencoder_X= LabelEncoder()
dataset_pred[:,0]= labelencoder_X.fit_transform(dataset_pred[:,0])
dataset_pred[:,1]= labelencoder_X.fit_transform(dataset_pred[:,1])
dataset_pred[:,3]= labelencoder_X.fit_transform(dataset_pred[:,3])
dataset_pred[:,4]= labelencoder_X.fit_transform(dataset_pred[:,4])


from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer

# converting variable property area using hot key encoder 
ct = ColumnTransformer([('encoder', OneHotEncoder(), [10])], remainder='passthrough')

new_pred = np.array(ct.fit_transform(dataset_pred))

# feature scaling of variables

from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()

new_pred = sc_X.fit_transform(new_pred)

##-----------********------------------#######---------------------------

# applying BEST Model on the test data for prediction and submission


z_pred = classifier3.predict(new_pred)


# write data frame file of prediction into csv for upload
z_pred = pd.DataFrame(z_pred)


z_pred.to_csv(r'/Users/a/Sanjay/Study/Projects/AV Loan Prediction/y_pred.csv', header = False, index = False)

sub_file.to_csv(r'/Users/a/Sanjay/Study/Projects/AV Loan Prediction/ID.csv', header = False, index = False)

# merge both the files in excel & submit 


####----------------------------------------------------------------------#######

"""# Visualization of Training set results 
from matplotlib.colors import ListedColormap
X_set,y_set = X_train,y_train
X1, X2 = np.meshgrid(np.arange(start = X_set[:,0].min() -1, stop = X_set[:,0].max() +1, step = 0.01),
                     np.arange(start = X_set[:,1].min() -1, stop = X_set[:,1].max() +1, step = 0.01))
plt.contourf(X1, X2, classifier.predict(np.array([X1.ravel(),X2.ravel()]).T).reshape(X1.shape),
              alpha = 0.75, cmap = ListedColormap(('red','green'))) 
plt.xlim(X1.min(), X1.max())
plt.ylim(X2.min(),X2.max())
for i,j in enumerate (np.unique(y_set)):
    plt.scatter(X_set[y_set == j,0], X_set[y_set == j,1],
                c= ListedColormap(('yellow','blue')) (i),label= j)
 
plt.title('Logistic Regression (Training)') 
plt.legend()
plt.show()  
"""












