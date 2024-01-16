import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')

################################################################################
# Problem 1.1
################################################################################

def deidentify(df):
    df = df.drop(columns=['Name','SSN'])
    return df

def pii(df):
    df = df.filter(['Name','SSN','DOB','Zip'])
    return df

def link_attack(deid_df, pii_df):
    pii_df = pii_df.drop_duplicates(subset=['Zip', 'DOB'], keep=False)
    #print(pii_df.shape)
    deid_df = deid_df.drop_duplicates(subset=['Zip', 'DOB'], keep=False)
    #print(deid_df.shape)
    df = pd.DataFrame.merge(deid_df, pii_df, on=['DOB','Zip'], how='inner')
    return df

################################################################################
# Problem 1.2
################################################################################

#def: k-anon
# if data indistinguishable from k-1 individuals
def is_k_anon(df, cols, k):
    df = df.groupby(by=cols,sort=False,as_index=False).size().reset_index().rename(columns={0:'count'})
    k_check = df['count'].min()
    return (k_check >= k)

################################################################################
# Problem 1.3
################################################################################

def num_bachelors(df):
    num_bach =  ((df['Education'].values) == 'Bachelors').sum()
    return num_bach

def laplace_mech(query, sensitivity, epsilon):
    b = sensitivity/epsilon
    return query + np.random.laplace(scale=b)

def compare_q(epsilon):
    qs = np.zeros(10000)
    for x in np.arange(200.0, 202.0, 1.0):
        for y in range(10000):
            qs[y] = laplace_mech(x, 1.0, epsilon)
        plt.hist(qs, bins = 50,  label=str(x))
    plt.legend(loc='upper right')
    plt.show()

def prob3():
    df = pd.read_csv("adult_with_pii.csv")
    sensitivity = 1.0
    base_q = 200.0
    print("base_q: " + str(base_q))
    print("sensitivity: " + str(sensitivity))
    qs = np.zeros(10000)
    eps = np.array([0.5,1.0,10.0])
    for ep in eps:
        print("For e = " + str(ep) + "...")
        for q in range(10000):
            qs[q] = laplace_mech(base_q, 1.0, ep)
        plt.hist(qs, bins = 50,  label=str(ep))
    plt.legend(loc='upper right')
    plt.show()
    return None
################################################################################
# Problem 1.5
################################################################################

def plot_error():
    df = pd.read_csv("adult_with_pii.csv")
    sensitivity = 1.0
    base_q = num_bachelors(df)
    print("base_q: " + str(base_q))
    print("sensitivity: " + str(sensitivity))
    qs = np.zeros(10000)
    eps = np.array([0.5, 1.0, 10.0])
    for ep in eps:
        print("For e = " + str(ep) + "...")
        for q in range(10000):
            qs[q] = abs(base_q - laplace_mech(base_q, 1.0, ep))
        plt.hist(qs, bins = 20,  label=str(ep))
    plt.legend(loc='upper right')
    plt.show()
    return None

# driver/test code below here
if __name__ == "__main__":
    prob3()
    compare_q(1.0)
    compare_q(10.0)
    plot_error()
    '''
    df = pd.read_csv("adult_with_pii.csv")

    print("**ORIG***")
    print(df.head)

    print("****DEID***")
    deid = deidentify(df)

    
    print(deid.head)
    
    print("***PII***")
    pii = pii(df)
    print(pii.head)

    print("***LINK***")
    link = link_attack(deid, pii)
    print(link.head)
    
    cols = ['Education']
    print("***K-ANON***")
    for x in range(1,12):
        print("Testing for " + str(x) +"-anonymity...")
        k_anon = is_k_anon(deid,cols,x)
        print(k_anon)

    print("***Num_Bach***")
    num_bach = num_bachelors(df)
    
    print("***Leplace***")


    #problem 4
    compare_q(0.50)

    plot_error()
    '''
    #prob 3