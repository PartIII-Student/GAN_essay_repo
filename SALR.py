import tensorflow as tf
import numpy as np
import pandas as pd
import os
import seaborn as sns
import matplotlib.pyplot as plt
import time
from tqdm import tqdm
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'


# Structure: in each trial generate parameters, then for number_of_epochs
# generate a batch of size 'batch_size' each time from the input distribution and the real distribution
# and train the GAN on it
number_of_trails = 1000
number_of_epochs = 50000
batch_size = 2000
hidden_layer_size_d = 12
hidden_layer_size_g = 6

# define actual distribution
real_mean = 6
real_sd = 1


# discriminator and generator NNs
def discriminator(input, parameters):
	pre_1 = tf.add(tf.matmul(tf.to_float(input), parameters[0]), parameters[1])
	activ_1 = tf.nn.relu(pre_1)
	pre_2 = tf.add(tf.matmul(activ_1, parameters[2]), parameters[3])
	activ_2 = tf.nn.relu(pre_2)
	pre_3 = tf.add(tf.matmul(activ_2, parameters[4]), parameters[5])
	output = tf.sigmoid(pre_3)
	return output


def generator(input, parameters):
	pre_1 = tf.add(tf.matmul(tf.to_float(input), parameters[0]), parameters[1])
	activ_1 = tf.tanh(pre_1)
	output = tf.add(tf.matmul(activ_1, parameters[2]), parameters[3])
	return output


# Create weights and biases variables
weight_d_1 = tf.Variable(tf.random_uniform([1, hidden_layer_size_d], minval=-1, maxval=1, dtype=tf.float32))
bias_d_1 = tf.Variable(tf.random_uniform([hidden_layer_size_d], minval=-1, maxval=1, dtype=tf.float32))
weight_d_2 = tf.Variable(tf.random_uniform([hidden_layer_size_d, hidden_layer_size_d], minval=-1, maxval=1, dtype=tf.float32))
bias_d_2 = tf.Variable(tf.random_uniform([hidden_layer_size_d], minval=-1, maxval=1, dtype=tf.float32))
weight_d_3 = tf.Variable(tf.random_uniform([hidden_layer_size_d, 1], minval=-1, maxval=1, dtype=tf.float32))
bias_d_3 = tf.Variable(tf.random_uniform([1], minval=-1, maxval=1, dtype=tf.float32))

d_parameters = [weight_d_1,bias_d_1, weight_d_2, bias_d_2, weight_d_3, bias_d_3]

weight_g_1 = tf.Variable(tf.random_uniform([1, hidden_layer_size_g], minval=-1, maxval=1, dtype=tf.float32))
bias_g_1 = tf.Variable(tf.random_uniform([hidden_layer_size_g], minval=-1, maxval=1, dtype=tf.float32))
weight_g_2 = tf.Variable(tf.random_uniform([hidden_layer_size_g, 1], minval=-1, maxval=1, dtype=tf.float32))
bias_g_2 = tf.Variable(tf.random_uniform([1], minval=-1, maxval=1, dtype=tf.float32))

g_parameters = [weight_g_1,bias_g_1, weight_g_2, bias_g_2]



# losses
real_dist_placeholder = tf.placeholder(tf.float32, shape=(None, 1))
generator_input_placeholder = tf.placeholder(tf.float32, shape=(None, 1))
with tf.variable_scope("Discriminator") as scope:
	d_output_real = discriminator(real_dist_placeholder, d_parameters)
	scope.reuse_variables()
	d_output_fake = discriminator(generator(generator_input_placeholder, g_parameters), d_parameters)
loss_d = tf.reduce_mean(-tf.log(d_output_real) - tf.log(1 - d_output_fake))
loss_g = tf.reduce_mean(tf.log(1-d_output_fake))

# Score Adaptive Learning Rate
phi_g = tf.placeholder(tf.float32)
phi_d = tf.placeholder(tf.float32)
gamma_g = tf.placeholder(tf.float32)
gamma_d = tf.placeholder(tf.float32)
psi_1 = tf.log((gamma_d - gamma_g - (2*phi_g) - phi_d)/(-gamma_d+gamma_g-phi_d))/tf.log(16.0)
psi = tf.maximum(0.0, psi_1)

V = tf.minimum(tf.reduce_mean(tf.log(d_output_real)+tf.log(1-d_output_fake)),0)

learning_rate_d = gamma_d-phi_d*tf.tanh(psi*V)
learning_rate_g = gamma_g + phi_g*(1 + tf.tanh(psi*V))

# Train step

train_g = tf.train.GradientDescentOptimizer(learning_rate_g).minimize(loss_g, var_list=g_parameters)
train_d = tf.train.GradientDescentOptimizer(learning_rate_d).minimize(loss_d, var_list=d_parameters)


for it in range(1,number_of_trails+1):
	# sample parameters
	gamma_vec = np.random.uniform(0.0000001,0.1,4)
	phi_vec = np.random.uniform(0.0, 0.1, 4)
	phi_vec[0] = 0.0

	res_matrix = np.zeros((len(gamma_vec) * len(phi_vec), batch_size))
	gamma_out_vec, phi_out_vec = np.zeros((len(gamma_vec) * len(phi_vec))), np.zeros((len(gamma_vec) * len(phi_vec)))

	row =0
	for i, p in enumerate(phi_vec):
		for j, k in enumerate(gamma_vec):
			print 'Trial: {}/{}'.format(it,number_of_trails)
			print 'Step: {}/{}'.format(row+1, len(gamma_vec) * len(phi_vec))
			print 'Phi: {0}'.format(p)
			print 'Gamma: {0}'.format(k)

			with tf.Session() as sess:
				tf.global_variables_initializer().run()
				# writer = tf.summary.FileWriter('./graphs', sess.graph)
				for step in tqdm(range(1, number_of_epochs+1)):
					generator_input = np.random.uniform(0, 1, (batch_size, 1))
					real_dist = np.random.normal(real_mean, real_sd, (batch_size, 1))

					sess.run(train_d, feed_dict={real_dist_placeholder: real_dist,
											 generator_input_placeholder: generator_input, phi_g: p,phi_d:p,
											 gamma_g:k,gamma_d:k })
					sess.run(train_g, feed_dict={real_dist_placeholder: real_dist,
											 generator_input_placeholder: generator_input, phi_g: p,phi_d:p,
											 gamma_g:k,gamma_d:k })

				generator_input = np.random.uniform(0, 1, (batch_size, 1))
				real_dist = np.random.normal(real_mean, real_sd, (batch_size, 1))

				generated = sess.run(generator(generator_input,g_parameters))
				res_matrix[row] = generated.reshape(batch_size)
				gamma_out_vec[row] = k
				phi_out_vec[row] = p
				row = row + 1

				print 'Mean of generated sample: {0}'.format(np.mean(generated))
				print 'Standard Deviation of generated sample: {0}'.format(np.std(generated))


				# writer.close()

				# sns.distplot(generated, hist=False, rug=False)
				# sns.distplot(real_dist, hist=False, rug=False)
				# plt.show()


	res_dataframe = pd.DataFrame(data=res_matrix.astype(float))
	gamma_dataframe = pd.DataFrame(data=gamma_out_vec.astype(float))
	phi_dataframe = pd.DataFrame(data=phi_out_vec.astype(float))

	output_dataframe1 = pd.concat([gamma_dataframe.reset_index(drop=True), phi_dataframe], axis=1)
	output_dataframe2 = pd.concat([output_dataframe1.reset_index(drop=True), res_dataframe], axis=1)

	script_path = os.path.abspath(__file__)
	script_dir = os.path.split(script_path)[0]
	file = os.path.join(script_dir,'SALR_data/gd/output.csv')

	with open(file, 'a') as f:
		output_dataframe2.to_csv(f, sep=',', header=False, float_format='%.9f', index=False)







