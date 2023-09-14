# A3: Kernel Ridge Regression and Bootstrap Methods


import numpy as np
import matplotlib.pyplot as plt
import random as rand
import scipy.optimize
from scipy.optimize import minimize
import scipy.linalg as la
from scipy.optimize import LinearConstraint
from scipy.optimize import NonlinearConstraint


## Define the target function
def funk(x):
    return 4*np.sin(np.pi*x)*np.cos(6*np.pi*(x)**2)

## Generate synthetic data
'''Generating data'''
n=30
global x_a3,y_a3
x_a3=np.random.rand(n)
e_a3=np.random.randn(n)

y_a3=funk(x_a3)+e_a3

## Plot the generated data and the true function
plt.figure(figsize=(8,5),dpi=100)
x_shape=np.linspace(0,1,10000)
y_shape=funk(x_shape)
plt.plot(x_shape,y_shape,'r')
plt.plot(x_a3,y_a3,'o', color='black')
plt.show()

## Define polynomial kernel
def kernel_poly(x,z,d):
    return (1+x.T*z)**d

## Define RBF kernel
def kernel_rbf(x,z,gamma):
    return np.exp(-gamma*((x.T-z)**2))


## Perform Kernel Ridge Regression
def kernel_ridge_regression(K,y,lamb):
    I=np.identity(np.size(y))
    return np.linalg.pinv(K+lamb*I)@y

## Additional Functions for Kernel Ridge Regression Project
## Function to calculate predicted values using a polynomial kernel

def poly_predicted_funk(x,alpha,X,hyperparameter):
    ans=0
    for i in range(np.size(X)):
        #print(i,'   ',alpha[i],'    ',kernel_poly(X[i],x,hyperparameter))
        ans+=alpha[i]*kernel_poly(X[i],x,hyperparameter)
    return and



## Function to calculate predicted values using an RBF kernel
def rbf_predicted_funk(x,alpha,X,hyperparameter):
    ans=0
    for i in range(np.size(X)):
        #print(alpha[i],'    ',kernel_rbf(X[i],x,hyperparameter))
        ans+=alpha[i]*kernel_rbf(X[i],x,hyperparameter)
    return and

## Leave-One-Out Cross Validation (LOOCV) error function for polynomial kernel

def mini_poly_leave_one_out_error(innput):
    x=x_a3
    y=y_a3
    n=len(x)
    predi=np.zeros(n)
    hyperparameter,lamb=innput[0],innput[-1]
    e=0
    
    #print('alpha',alpha,'done')
    for i in range(n):
        
        
        
        A=list(range(0,n))
        A=np.array(A)
        A=A[A!=A[i]]
        #print('A',A)

    
        train_x=x[x!=x[i]]
        #print('train_x',train_x)
        train_y=y[y!=y[i]]
        
        '''making 29 by 29 K'''
        K=np.zeros((n-1,n-1))
    
        for ii in A:
            for jj in A:
                #print('ii',ii,'jj',jj)
                iii=np.where(A==ii)[0][0]
                jjj=np.where(A==jj)[0][0]
            
                K[iii,jjj]=kernel_poly(x[iii],x[jjj],int(hyperparameter))
                    
        '''Making alpha'''
        alpha=kernel_ridge_regression(K,train_y,lamb)  
        
        
        #poly_prediction[i]=poly_predicted_funk(x_a3[i],alpha,train_x,int(hyperparameter))

    
        '''error'''
        pre=poly_predicted_funk(x[i],alpha,train_x,hyperparameter)
        predi[i]=pre
        #print(pre)
        e+=(pre-y[i])**2
    return e/n#,predi


## Leave-One-Out Cross Validation (LOOCV) error function for RBF kernel
def mini_rbf_leave_one_out_error(innput):
    x=x_a3
    y=y_a3
    n=len(x)
    predi=np.zeros(n)
    hyperparameter,lamb=innput[0],innput[-1]
    e=0
    
    #print('alpha',alpha,'done')
    for i in range(n):
        
        A=list(range(0,n))
        A=np.array(A)
        A=A[A!=A[i]]
        #print('A',A)
    
        train_x=x[x!=x[i]]
        #print('train_x',train_x)
        train_y=y[y!=y[i]]
        
        '''making 29 by 29 K'''
        K=np.zeros((n-1,n-1))
    
        for ii in A:
            for jj in A:
                #print('ii',ii,'jj',jj)
                iii=np.where(A==ii)[0][0]
                jjj=np.where(A==jj)[0][0]
            
                K[iii,jjj]=kernel_rbf(x[iii],x[jjj],hyperparameter)
                    
        '''Making alpha'''
        alpha=kernel_ridge_regression(K,train_y,lamb)  
        #print('i is ',i,'alpha shape is',np.shape(alpha))
        
        #poly_prediction[i]=rbf_predicted_funk(x_a3[i],alpha,train_x,hyperparameter)

    
        '''error'''
        pre=rbf_predicted_funk(x_a3[i],alpha,train_x,hyperparameter)
        predi[i]=pre
        #print(pre)
        e+=(pre-y[i])**2
    return e/n#,predi

# A3. a)
## Polynomial Kernel Ridge Regression Optimization

'''POLY'''
x0=[40,1]
bnds=((30,50),(0,1))
poly_result=minimize(mini_poly_leave_one_out_error, x0,tol=1e-11, bounds=bnds)
poly_result

## RBF Kernel Ridge Regression Optimization
'''RBF'''
x0=[20,1]
bnds=((0,None),(0,None))
rbf_result=minimize(mini_rbf_leave_one_out_error, x0,tol=1e-10, bounds=bnds)
rbf_result


# A3. b)
## Title: Kernel Ridge Regression Predictions and Visualization
# Title: Kernel Ridge Regression Predictions and Visualization

# Initialization
x, y, n = x_a3, y_a3, len(x_a3)
rbf_prediction = np.zeros(n)  # Initialize RBF kernel predictions array
poly_prediction = np.zeros(n)  # Initialize Polynomial kernel predictions array

# Polynomial Kernel Predictions
# Extracting optimized hyperparameters for polynomial kernel
hyperparameter_poly, lamb_poly = poly_result.x

# Note: The hyperparameter for the polynomial kernel degree must be an integer.
# Hence, it's often rounded to the nearest integer in some implementations.
#hyperparameter_poly = int(hyperparameter_poly)

# Construct the Polynomial Kernel matrix K_poly
K_poly = np.zeros((n, n))
for i in range(n):
    for j in range(n):
        K_poly[i, j] = kernel_poly(x[i], x[j], hyperparameter_poly)

# Calculate the alpha coefficients for polynomial kernel
alpha_poly = kernel_ridge_regression(K_poly, y, lamb_poly)

# Make predictions using the Polynomial Kernel
for i in range(n):
    poly_prediction[i] = poly_predicted_funk(x[i], alpha_poly, x, hyperparameter_poly)

# RBF Kernel Predictions
# Extracting optimized hyperparameters for RBF kernel
hyperparameter_rbf, lamb_rbf = rbf_result.x

# Construct the RBF Kernel matrix K_rbf
K_rbf = np.zeros((n, n))
for i in range(n):
    for j in range(n):
        K_rbf[i, j] = kernel_rbf(x[i], x[j], hyperparameter_rbf)

# Calculate the alpha coefficients for RBF kernel
alpha_rbf = kernel_ridge_regression(K_rbf, y, lamb_rbf)

# Make predictions using the RBF Kernel
for i in range(n):
    rbf_prediction[i] = rbf_predicted_funk(x[i], alpha_rbf, x, hyperparameter_rbf)

# Plotting Predictions and Actual Data
plt.figure(figsize=(8, 5), dpi=110)
plt.plot(x_a3, y_a3, 'o', color='red', label='actual data')  # Plot actual data points
plt.plot(x_a3, poly_prediction, 'o', color='black', label='poly predictions')  # Plot Polynomial Kernel predictions
plt.plot(x_a3, rbf_prediction, 'o', color='blue', label='rbf predictions')  # Plot RBF Kernel predictions
plt.plot(x_shape, y_shape, 'y', label='real function')  # Plot the underlying true function
plt.legend(loc="best")  # Show legend

plt.show()  # Display the plot


# A3. c)
## Assumed functions and data: kernel_poly, kernel_rbf, kernel_ridge_regression, x_a3, y_a3
B=300
n=30
x_draw=np.zeros((n,B))
y_draw=np.zeros((n,B))
forecast_rbf=np.zeros((n,B))
forecast_poly=np.zeros((n,B))

for b in range(B):
    
    '''Generating data'''
    for j in range(n):
        draw=rand.randrange(0,n)
        x_draw[j,b]=x_a3[draw]
        y_draw[j,b]=y_a3[draw]
    
    
    
    x=x_draw[:,b]
    y=y_draw[:,b]
    
    
    
    '''POLY'''
    
    K_poly=np.zeros((n,n))
    for i in range(n):
        for j in range(n):   
            K_poly[i,j]=kernel_poly(x[i],x[j],hyperparameter_poly)
            
    alpha_poly=kernel_ridge_regression(K_poly,y,lamb_poly)  
    for i in range(n):
        forecast_poly[i,b]=poly_predicted_funk(x[i],alpha_poly,x,hyperparameter_poly)

    
    
    '''RBF'''
    K_rbf=np.zeros((n,n))
    for i in range(n):
        for j in range(n):   
            K_rbf[i,j]=kernel_rbf(x[i],x[j],hyperparameter_rbf)
    alpha_rbf=kernel_ridge_regression(K_rbf,y,lamb_rbf)  
    for i in range(n):
        forecast_rbf[i,b]=rbf_predicted_funk(x[i],alpha_rbf,x,hyperparameter_rbf)

    
'''Making the confidence intervals'''

bootstrap_rbf=np.zeros((n,2))
bootstrap_poly=np.zeros((n,2))

for i in range(n):
    
    '''Poly'''
    order_y=np.zeros(B*n)
    ii=0
    for b in range(B):
        for j in range(n):
            if x_draw[j,b]==x_a3[i]:
                order_y[ii]+=forecast_poly[i,b]
                ii+=1
    order_y=order_y[0:ii]
    order=order_y
    order.sort()

    one=order[int(0.05*len(order))]
    two=order[int(0.95*len(order))]

    one=np.where(order==one)[0][0]
    two=np.where(order==two)[0][0]
    
    bootstrap_poly[i,0]=order[one]
    bootstrap_poly[i,1]=order[two]
    
    
    '''RBF'''
    order_y=np.zeros(B*n)
    ii=0
    for b in range(B):
        for j in range(n):
            if x_draw[j,b]==x_a3[i]:
                order_y[ii]+=forecast_rbf[i,b]
                ii+=1
    order_y=order_y[0:ii]
    order=order_y
    order.sort()
    

    one=order[int(0.05*len(order))]
    two=order[int(0.95*len(order))]

    one=np.where(order==one)[0][0]
    two=np.where(order==two)[0][0]
    
    bootstrap_rbf[i,0]=order[one]
    bootstrap_rbf[i,1]=order[two]
    
    
    
    
'''to get a good plot we need to sort them all now'''

Y1 = [x for _,x in sorted(zip(x_a3,bootstrap_poly[:,0]))]
Y2 = [x for _,x in sorted(zip(x_a3,bootstrap_poly[:,1]))]
Y3 = [x for _,x in sorted(zip(x_a3,bootstrap_rbf[:,0]))]
Y4 = [x for _,x in sorted(zip(x_a3,bootstrap_rbf[:,1]))]

X0=x_a3
Y0= [x for _,x in sorted(zip(x_a3,y_a3))]
X0.sort()

plt.figure(figsize=(9,7),dpi=100)
plt.plot(X0,Y0,'b--',color='black',label='actuall data')
plt.plot(X0,Y1,'r--',label='bootstrap_poly 5%')
plt.plot(X0,Y2,'g--',label='bootstrap_poly 95%')
plt.plot(X0,Y3,label='bootstrap_rbf 5%')
plt.plot(X0,Y4,label='bootstrap_rbf 95%')

plt.legend(loc="best")
plt.show()


# A3. d) 10-fold CV instead of leave-one-out
## Title: 10-Fold Cross-Validation and Data Generation in Kernel Ridge Regression

'''Generating data'''
n=300
np.random.seed(313)
global x_a3,y_a3
x_a3_d=np.random.rand(n)
e_a3_d=np.random.randn(n)

y_a3_d=funk(x_a3_d)+e_a3_d

x_a3=x_a3_d
y_a3=y_a3_d

plt.figure(figsize=(8,5),dpi=100)
x_shape=np.linspace(0,1,10000)
y_shape=funk(x_shape)
plt.plot(x_shape,y_shape,'r')
plt.plot(x_a3,y_a3,'o', color='black')
plt.show()

## Mini-Batch 10-Fold Cross-Validation for Polynomial Kernel Ridge Regression
# Title: Mini-Batch 10-Fold Cross-Validation for Polynomial Kernel Ridge Regression

def mini_poly_k_fold_error(innput):
    # Initialize variables and hyperparameters
    x=x_a3
    y=y_a3
    n=len(x)
    predi=np.zeros(n)
    hyperparameter,lamb=innput[0],innput[-1]
    e=0  # Error accumulator
    N=10  # Fold size
    
    # Loop through each fold in the 10-fold cross-validation
    for fold in range(int(n/10)):
        
        # Generate index sets for the training and test data
        All=np.array(list(range(0,n)))            
        if fold==0: 
            A_idx=list(range(0,n))[None:10]
        else: 
            A_idx=list(range(0,n))[fold*N:fold*N+N] 
        idx = np.ones(n,dtype=bool)
        idx[A_idx] = False
        A=All[idx]
        B=All[~idx]
        train_x=x[idx]
        train_y=y[idx]
        
        # Construct the kernel matrix K for polynomial kernel
        K=np.zeros((n-N,n-N))
        for ii in A:
            for jj in A:
                iii=np.where(A==ii)[0][0]
                jjj=np.where(A==jj)[0][0]
                K[iii,jjj]=kernel_poly(x[iii],x[jjj],int(hyperparameter))
        
        # Compute alpha using kernel ridge regression
        alpha=kernel_ridge_regression(K,train_y,lamb)  
        
        # Compute error for this fold
        for i in B:
            e+=(poly_predicted_funk(x[i],alpha,train_x,hyperparameter)-y[i])**2
            
    return e/(n/N)  # Return the average error


## Mini-Batch 10-Fold Cross-Validation for RBF Kernel Ridge Regression
def mini_rbf_k_fold_error(innput):
    # Initialize variables and hyperparameters
    x = x_a3
    y = y_a3
    n = len(x)
    predi = np.zeros(n)
    hyperparameter, lamb = innput[0], innput[-1]
    e = 0  # Error accumulator
    
    N = 10  # Fold size
    
    # Loop through each fold in the 10-fold cross-validation
    for fold in range(int(n / 10)):
        
        # Generate index sets for the training and test data
        All = np.array(list(range(0, n)))  
        if fold == 0: 
            A_idx = list(range(0, n))[None:10]
        else: 
            A_idx = list(range(0, n))[fold * N:fold * N + N]
        
        idx = np.ones(n, dtype=bool)
        idx[A_idx] = False
        A = All[idx]
        B = All[~idx]
        train_x = x[idx]
        train_y = y[idx]
        
        # Construct the kernel matrix K for RBF kernel
        K = np.zeros((n - N, n - N))
        for ii in A:
            for jj in A:
                iii = np.where(A == ii)[0][0]
                jjj = np.where(A == jj)[0][0]
                K[iii, jjj] = kernel_rbf(x[iii], x[jjj], int(hyperparameter))
        
        # Compute alpha using kernel ridge regression
        alpha = kernel_ridge_regression(K, train_y, lamb)  
        
        # Compute error for this fold
        for i in B:
            e += (rbf_predicted_funk(x[i], alpha, train_x, hyperparameter) - y[i]) ** 2
            
    return e / (n / N)  # Return the average error



## Title: Hyperparameter Optimization for Polynomial Kernel using K-Fold CV
'''K-Fold POLY'''
x0=[40,1]
bnds=((0,100),(0,1.5))
poly_result=minimize(mini_poly_k_fold_error, x0, bounds=bnds)
#poly_result


## Title: Hyperparameter Optimization for RBF Kernel using K-Fold CV
'''K-Fold RBF'''
x0=[227,3]
bnds=((0,300),(0,1.5))
rbf_result=minimize(mini_rbf_k_fold_error, x0, bounds=bnds)
#rbf_result



## Title: Kernel Ridge Regression Predictions with Optimal Hyperparameters
x_a3=x_a3_d
y_a3=y_a3_d

x=x_a3
y=y_a3
n=len(x)
rbf_prediction=np.zeros(n)
poly_prediction=np.zeros(n)

'''POLY'''
hyperparameter_poly,lamb_poly=poly_result.x


# Note: We use the closest integer for hyperparameter_poly as it has to be an integer
# This is common in certain mathematical optimizations


#hyperparameter_poly=int(hyperparameter_poly)
#hyperparameter,lamb=(10,3)
K_poly=np.zeros((n,n))
for i in range(n):
    for j in range(n):   
        K_poly[i,j]=kernel_poly(x[i],x[j],hyperparameter_poly)
alpha_poly=kernel_ridge_regression(K_poly,y,lamb_poly)  
for i in range(n):
    poly_prediction[i]=poly_predicted_funk(x[i],alpha_poly,x,hyperparameter_poly)

    

'''RBF'''
hyperparameter_rbf,lamb_rbf=rbf_result.x
#hyperparameter,lamb=(20,.9)
K_rbf=np.zeros((n,n))
for i in range(n):
    for j in range(n):   
        K_rbf[i,j]=kernel_rbf(x[i],x[j],hyperparameter_rbf)
alpha_rbf=kernel_ridge_regression(K_rbf,y,lamb_rbf)  
for i in range(n):
    rbf_prediction[i]=rbf_predicted_funk(x[i],alpha_rbf,x,hyperparameter_rbf)

    
# Plotting Results
plt.figure(figsize=(8,5),dpi=110)
#plt.plot(x_a3,y_a3,'o', color='red',label='actuall data')
plt.plot(x_a3,poly_prediction,'o', color='black',label='poly predictions')
plt.plot(x_a3,rbf_prediction,'o', color='blue',label='rbf predictions')
plt.plot(x_shape,y_shape,'y',label='real function')
plt.legend(loc="best")

plt.show()


## Title: Confidence Intervals for Kernel Ridge Regression via Bootstrap
B=30
n=300
x_draw=np.zeros((n,B))
y_draw=np.zeros((n,B))
forecast_rbf=np.zeros((n,B))
forecast_poly=np.zeros((n,B))

for b in range(B):
    
    '''Generating data'''
    for j in range(n):
        draw=rand.randrange(0,n)
        x_draw[j,b]=x_a3[draw]
        y_draw[j,b]=y_a3[draw]
    
    
    
    x=x_draw[:,b]
    y=y_draw[:,b]
    
    
    
    '''POLY'''
    
    K_poly=np.zeros((n,n))
    for i in range(n):
        for j in range(n):   
            K_poly[i,j]=kernel_poly(x[i],x[j],hyperparameter_poly)
            
    alpha_poly=kernel_ridge_regression(K_poly,y,lamb_poly)  
    for i in range(n):
        forecast_poly[i,b]=poly_predicted_funk(x[i],alpha_poly,x,hyperparameter_poly)

    
    
    '''RBF'''
    K_rbf=np.zeros((n,n))
    for i in range(n):
        for j in range(n):   
            K_rbf[i,j]=kernel_rbf(x[i],x[j],hyperparameter_rbf)
    alpha_rbf=kernel_ridge_regression(K_rbf,y,lamb_rbf)  
    for i in range(n):
        forecast_rbf[i,b]=rbf_predicted_funk(x[i],alpha_rbf,x,hyperparameter_rbf)

    
'''Making the confidence intervals'''

bootstrap_rbf=np.zeros((n,2))
bootstrap_poly=np.zeros((n,2))

for i in range(n):
    
    '''Poly'''
    order_y=np.zeros(B*n)
    ii=0
    for b in range(B):
        for j in range(n):
            if x_draw[j,b]==x_a3[i]:
                order_y[ii]+=forecast_poly[i,b]
                ii+=1
    order_y=order_y[0:ii]
    order=order_y
    order.sort()
    
    
    
    if len(order)==0:
        #print('here x_draw[i,b]',x_draw[i,b])
        continue 

    one=order[int(0.05*len(order))]
    two=order[int(0.95*len(order))]

    one=np.where(order==one)[0][0]
    two=np.where(order==two)[0][0]
    
    bootstrap_poly[i,0]=order[one]
    bootstrap_poly[i,1]=order[two]
    
    
    '''RBF'''
    order_y=np.zeros(B*n)
    ii=0
    for b in range(B):
        for j in range(n):
            if x_draw[j,b]==x_a3[i]:
                order_y[ii]+=forecast_rbf[i,b]
                ii+=1
    order_y=order_y[0:ii]
    order=order_y
    order.sort()
    
    
    
    one=order[int(0.05*len(order))]
    two=order[int(0.95*len(order))]

    one=np.where(order==one)[0][0]
    two=np.where(order==two)[0][0]
    
    bootstrap_rbf[i,0]=order[one]
    bootstrap_rbf[i,1]=order[two]
    
    
    
    
'''to get a good plot we need to sort them all now'''

Y1 = [x for _,x in sorted(zip(x_a3,bootstrap_poly[:,0]))]
Y2 = [x for _,x in sorted(zip(x_a3,bootstrap_poly[:,1]))]
Y3 = [x for _,x in sorted(zip(x_a3,bootstrap_rbf[:,0]))]
Y4 = [x for _,x in sorted(zip(x_a3,bootstrap_rbf[:,1]))]

X0=x_a3
Y0= [x for _,x in sorted(zip(x_a3,y_a3))]
X0.sort()

plt.figure(figsize=(9,7),dpi=100)
plt.plot(X0,Y0,'b--',color='black',label='actuall data')
plt.plot(X0,Y1,'r--',label='bootstrap_poly 5%')
plt.plot(X0,Y2,'g--',label='bootstrap_poly 95%')
plt.plot(X0,Y3,label='bootstrap_rbf 5%')
plt.plot(X0,Y4,label='bootstrap_rbf 95%')

plt.legend(loc="best")
plt.show()


# A3. e)
## Kernel Ridge Regression Predictions on 1000 Data Points
'''Generating data'''
n=1000
global x_a3,y_a3
x_a3=np.random.rand(n)
e_a3=np.random.randn(n)

y_a3=funk(x_a3)+e_a3


x=x_a3
y=y_a3
n=len(x)
rbf_prediction=np.zeros(n)
poly_prediction=np.zeros(n)

'''POLY'''
hyperparameter_poly,lamb_poly=poly_result.x

'''I pick the closest int for the d since it got to be integer
(that is one why in math they do integer optimization)'''

#hyperparameter_poly=int(hyperparameter_poly)
#hyperparameter,lamb=(10,3)
K_poly=np.zeros((n,n))
for i in range(n):
    for j in range(n):   
        K_poly[i,j]=kernel_poly(x[i],x[j],hyperparameter_poly)
alpha_poly=kernel_ridge_regression(K_poly,y,lamb_poly)  
for i in range(n):
    poly_prediction[i]=poly_predicted_funk(x[i],alpha_poly,x,hyperparameter_poly)

    

'''RBF'''
hyperparameter_rbf,lamb_rbf=rbf_result.x
#hyperparameter,lamb=(20,.9)
K_rbf=np.zeros((n,n))
for i in range(n):
    for j in range(n):   
        K_rbf[i,j]=kernel_rbf(x[i],x[j],hyperparameter_rbf)
alpha_rbf=kernel_ridge_regression(K_rbf,y,lamb_rbf)  
for i in range(n):
    rbf_prediction[i]=rbf_predicted_funk(x[i],alpha_rbf,x,hyperparameter_rbf)

    
    
plt.figure(figsize=(8,5),dpi=110)
#plt.plot(x_a3,y_a3,'o', color='red',label='actuall data')
plt.plot(x_a3,poly_prediction,'o', color='black',label='poly predictions')
plt.plot(x_a3,rbf_prediction,'o', color='blue',label='rbf predictions')
plt.plot(x_shape,y_shape,'y',label='real function')
plt.legend(loc="best")

plt.show()



B=300
n=1000
x_draw=np.zeros((n,B))
y_draw=np.zeros((n,B))

error=np.zeros(B)

for b in range(B):
    
    '''Generating data'''
    for j in range(n):
        draw=rand.randrange(0,n)
        x_draw[j,b]=x_a3[draw]
        y_draw[j,b]=y_a3[draw]
    
    x=x_draw[:,b]
    y=y_draw[:,b]
    
    
    '''POLY'''
    
    K_poly=np.zeros((n,n))
    for i in range(n):
        for j in range(n):   
            K_poly[i,j]=kernel_poly(x[i],x[j],hyperparameter_poly)
            
    alpha_poly=kernel_ridge_regression(K_poly,y,lamb_poly)  
    for i in range(n):
        error[b]+=(poly_predicted_funk(x[i],alpha_poly,x,hyperparameter_poly)-y[i])**2

    
    
    '''RBF'''
    K_rbf=np.zeros((n,n))
    for i in range(n):
        for j in range(n):   
            K_rbf[i,j]=kernel_rbf(x[i],x[j],hyperparameter_rbf)
    alpha_rbf=kernel_ridge_regression(K_rbf,y,lamb_rbf)  
    for i in range(n):
        error[b]-=(rbf_predicted_funk(x[i],alpha_rbf,x,hyperparameter_rbf)-y[i])**2
error/=n


error.sort()
print('%5 is ',error[int(B*0.5)],'%95 is ',error[int(B*0.95)])
print('so no zero in this range and we can NOT reject the hypothesis that they are NOT the same')



