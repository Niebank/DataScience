import numpy as np

MODEL_PATH = 'model.nn'
DATASET    = 'data.csv'

class TurnPredictor:
    def __init__(self):
        self.model = None
    
    def load(self):
        from keras.models import load_model
        self.model = load_model(MODEL_PATH)

    def predict(self, lane, speed, accel, phase=0):
        data = np.array([[lane, speed, accel, phase]])
        prediction = self.model.predict(data)[0][0] > 0.5
        return prediction

    def evaluate(self):
        dataset = np.loadtxt(DATASET, delimiter=',', skiprows=1)
        X = dataset[:,1:5]
        y = dataset[:,0]
        # X = self.sc.fit_transform(X)

        pred_y = self.model.predict(X)
        pred_y = pred_y.flatten()

        # count the number of non-zero predictions (turn)
        print(f'Min: {min(pred_y)}, Max: {max(pred_y)}')
        print('Number of turns predicted: ', sum(pred_y > 0.5))
        print(f'Number of actual turns: {int(sum(y))} out of {len(y)} observations')

    def train(self):
        from keras.models import Sequential
        from keras.layers import Dense
        from sklearn.model_selection import train_test_split

        dataset = np.loadtxt(DATASET, delimiter=',', skiprows=1)
        X = dataset[:,1:5]
        y = dataset[:,0]
        # X = self.sc.fit_transform(X)

        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1666666666)
        
        model = Sequential()
        model.add(Dense(4, input_dim=4, activation='relu'))
        model.add(Dense(8, activation='relu'))
        model.add(Dense(1, activation='sigmoid'))
        model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
        model.fit(X_train, y_train, epochs=200, batch_size=10)

        model.save(MODEL_PATH)
        _, accuracy = model.evaluate(X_test, y_test)
        print('Accuracy:', accuracy)
        self.model = model


if __name__ == '__main__':
    nn = TurnPredictor()
    #nn.load()
    nn.train()
    nn.evaluate()
