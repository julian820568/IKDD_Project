import pandas as P
from sklearn import preprocessing
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import ExtraTreesRegressor

def csv_load(datafile):
    return P.read_csv(datafile)

def time_categorize(data):
    tmp = P.DataFrame(data.datetime.str.split(' ').tolist(), columns = "tmp2 tmp3".split())
    tmp2 = P.DataFrame(tmp.tmp2.str.split('-').tolist(), columns="year month day".split())
    tmp3 = P.DataFrame(tmp.tmp3.str.split(':').tolist(), columns = "hour minute second".split())
    data['year'] = tmp2['year']
    data['month'] = tmp2['month']
    data['day'] = tmp2['day']
    data['hour'] = tmp3['hour'].astype(int)
    return data

def DecisionTree():
    value = DecisionTreeRegressor()
    return value

def ExtraTree():
    value = ExtraTreesRegressor(n_estimators=500)
    return value

def predict(value, train, test, attribute, result):
    value.fit(train[attribute], train[result])
    with open("/home/hw5/result/" + "tttt.csv", 'wb') as f:
        f.write("datetime,count\n")
        for index, value in enumerate(list(value.predict(test[attribute]))):
            f.write("%s,%s\n" % (test['datetime'].loc[index], int(value)))

def main():
    train = csv_load("/home/hw5/project/data/" + "train.csv")
    test = csv_load("/home/hw5/project/data/" + "test.csv")
    train = time_categorize(train)
    test = time_categorize(test)
    result = 'count'
    attribute = [col for col in train.columns if col not in ['datetime', 'casual', 'registered', 'count']]
    #value = DecisionTree()
    value = ExtraTree()
    predict(value, train, test, attribute, result)

if __name__ == "__main__":
    main()
