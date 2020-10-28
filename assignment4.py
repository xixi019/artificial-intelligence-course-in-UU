import tensorflow as tf
from tensorflow.keras import datasets, layers, models
import matplotlib.pyplot as plt
import pandas as pd
from pandas import DataFrame
import numpy as np


train_df = pd.read_csv("trainset.csv")
test_df = pd.read_csv("testset.csv")

train_labels = tf.reshape(tf.convert_to_tensor(train_df["label"].values), shape = (len(train_df), 1))
train_images =  np.zeros(shape=(len(train_df), 28, 28, 1))
for idx in range(len(train_df)):
    for i in range(28):
        for j in range(28):
            train_images[idx, i, j, 0] = train_df[f'pixel{(i*28)+j}'][idx]
train_images = tf.convert_to_tensor(train_images)


test_images =  np.zeros(shape=(len(test_df), 28, 28, 1))
for idx in range(len(test_df)):
    for i in range(28):
        for j in range(28):
            test_images[idx, i, j, 0] = test_df[f'pixel{(i*28)+j}'][idx]
test_images = tf.convert_to_tensor(test_images)

train_images, test_images = train_images / 255.0, test_images / 255.0

model = models.Sequential()
model.add(layers.Conv2D(28, (3, 3), activation='relu', input_shape=(28, 28, 1)))
model.add(layers.MaxPooling2D((2, 2)))
model.add(layers.Conv2D(64, (3, 3), activation='relu'))
model.add(layers.MaxPooling2D((2, 2)))
model.add(layers.Conv2D(64, (3, 3), activation='relu'))

model.summary()
model.add(layers.Flatten())
model.add(layers.Dense(64, activation='relu'))
model.add(layers.Dense(10))

model.add(layers.Flatten())
model.add(layers.Dense(64, activation='relu'))
model.add(layers.Dense(10))

model.compile(optimizer='adam',
              loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
              metrics=['accuracy'])


history = model.fit(train_images, train_labels, epochs=10)

predict = model.predict(test_images)
predict = np.argmax(predict, axis=1)

result = {"ImageID":[i for i in range(1, len(predict)+1)], "Label":predict}
result = DataFrame(result)
result.to_csv("result.csv")
