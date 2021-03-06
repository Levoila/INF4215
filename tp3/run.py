import os
os.environ['THEANO_FLAGS'] = 'floatX=float32,device=gpu1,lib.cnmem=0.5'

import theano
import theano.tensor as T
import numpy
from theano.tensor.nnet import conv2d
from theano.tensor.signal import downsample
from theano.tensor.shared_randomstreams import RandomStreams
import cPickle
import gzip
import matplotlib.pyplot as plt
import math
import sys

#From https://github.com/Theano/Theano/blob/master/theano/tensor/nnet/nnet.py#L2187
#to be compatible with theano 0.7.0
def relu(x):
	return 0.417 * (x + 1.4 * abs(x))

#inspired by http://deeplearning.net/tutorial/lenet.html
# and http://deeplearning.net/tutorial/mlp.html
class HiddenLayer(object):
    def __init__(self, rng, input, n_in, n_out, W=None, b=None,
                 activation=relu):

        self.n_out = n_out
        self.input = input

        if W is None:
			W_bound = numpy.sqrt(3) * numpy.sqrt(1.0 / n_in)
			self.W = theano.shared(numpy.asarray(rng.uniform(low=-W_bound, high=W_bound, size=(n_in, n_out)), dtype=theano.config.floatX), borrow=True)

        if b is None:
            self.b = theano.shared(numpy.zeros((n_out,), dtype=theano.config.floatX) + 0.1, name='b', borrow=True)

        lin_output = T.dot(input, self.W) + self.b
        self.output = (
            lin_output if activation is None
            else activation(lin_output)
        )

        self.params = [self.W, self.b]

class LogisticRegression(object):
    def __init__(self, rng, input, n_in, n_out):
        W_bound = numpy.sqrt(3) * numpy.sqrt(1.0 / n_in)
        self.W = theano.shared(numpy.asarray(rng.uniform(low=-W_bound, high=W_bound, size=(n_in, n_out)), dtype=theano.config.floatX), borrow=True)

        self.b = theano.shared(numpy.zeros((n_out,), dtype=theano.config.floatX) + 0.1, name='b', borrow=True)

        self.p_y_given_x = T.nnet.softmax(T.dot(input, self.W) + self.b)
        self.y_pred = T.argmax(self.p_y_given_x, axis=1)

        self.params = [self.W, self.b]

        self.input = input

    def negative_log_likelihood(self, y):
        return -T.mean(T.log(self.p_y_given_x)[T.arange(y.shape[0]), y])

    def errors(self, y):
        return T.mean(T.neq(self.y_pred, y))

class ConvPoolLayer(object):

    def __init__(self, rng, input, filter_shape, image_shape, poolsize=(2, 2)):

        assert image_shape[1] == filter_shape[1]
        self.input = input


        fan_in = numpy.prod(filter_shape[1:])
        fan_out = (filter_shape[0] * numpy.prod(filter_shape[2:]) // numpy.prod(poolsize))

        #Glorot initialization http://andyljones.tumblr.com/post/110998971763/an-explanation-of-xavier-initialization
        W_bound = numpy.sqrt(3) * numpy.sqrt(1.0 / fan_in)
        self.W = theano.shared(
            numpy.asarray(
                rng.uniform(low=-W_bound, high=W_bound, size=filter_shape),
                dtype=theano.config.floatX
            ),
            borrow=True
        )

        self.b = theano.shared(numpy.zeros((filter_shape[0],), dtype=theano.config.floatX) + 0.1, borrow=True)

        conv_out = conv2d(
            input=input,
            filters=self.W,
            filter_shape=filter_shape,
            image_shape=image_shape
        )

        pool_out = downsample.max_pool_2d(
			input=conv_out,
			ds=poolsize,
			ignore_border=True
        )

        self.output = relu(pool_out + self.b.dimshuffle('x', 0, 'x', 'x'))

        self.params = [self.W, self.b]

        self.input = input


class Net:
	def __init__(self, train_x, train_y, valid_x, valid_y, test_x, test_y, batchSize):
		rng = numpy.random.RandomState(42)

		self.train_x = theano.shared(train_x.astype('float32'))
		self.train_y = theano.shared(train_y.astype('int32'))
		self.valid_x = theano.shared(valid_x.astype('float32')).reshape((valid_x.shape[0],1,28,28))
		self.valid_y = theano.shared(valid_y.astype('int32'))
		self.test_x = theano.shared(test_x.astype('float32')).reshape((test_x.shape[0],1,28,28))
		self.test_y = theano.shared(test_y.astype('int32'))

		x = T.matrix()
		y = T.ivector()
		index = T.lscalar()
		learningRate = T.scalar()
		L1_reg = 0.0
		L2_reg = 0.0

		random_stream = RandomStreams(seed=420)
		indices = random_stream.random_integers((batchSize,), low=0, high=train_x.shape[0]-1)
		x = self.train_x.take(indices, axis=0)
		y = self.train_y.take(indices, axis=0)

		layer0Input = x.reshape((batchSize,1,28,28))

		layer0 = ConvPoolLayer(
			rng=rng,
			input=layer0Input,
			filter_shape=(64,1,3,3),
			image_shape=(None,1,28,28),
			poolsize=(2,2)
		)

		layer1 = ConvPoolLayer(
			rng=rng,
			input=layer0.output,
			filter_shape=(128,64,3,3),
			image_shape=(None,64,13,13),
			poolsize=(2,2)
		)

		layer1Out = layer1.output.flatten(2)

		layer2 = HiddenLayer(
			rng=rng,
			input=layer1Out,
			n_in=128*5*5,
			n_out=512,
			activation=relu
		)

		layer3 = LogisticRegression(
			rng=rng,
			input=layer2.output,
			n_in=layer2.n_out,
			n_out=10
		)

		L1 = abs(layer0.W).sum() + abs(layer1.W).sum() + abs(layer2.W).sum() + abs(layer3.W).sum()
		L2 = (layer0.W**2).sum() + (layer1.W**2).sum() + (layer2.W**2).sum() + (layer3.W**2).sum()
		cost = layer3.negative_log_likelihood(y) + L1_reg * L1 + L2_reg * L2

		self.test_model = theano.function(
			[index],
			layer3.errors(y),
			givens={
				layer0Input: self.test_x[index * 1000:(index+1)*1000,:,:,:],
				y: self.test_y[index * 1000:(index+1)*1000]
			}
		)

		self.validate_model = theano.function(
			[index],
			[layer3.errors(y), cost],
			givens={
				layer0Input: self.valid_x[index * 1000:(index+1)*1000,:,:,:],
				y: self.valid_y[index * 1000:(index+1)*1000]
			}
		)
		
		self.forward = theano.function([layer0Input], [layer3.p_y_given_x])
		
		self.params = layer3.params + layer2.params + layer1.params + layer0.params
		updates = self.rmsProp(cost, self.params, 0.7, 0.01, learningRate)
		self.train_model = theano.function(
			[learningRate],
			cost,
			updates=updates
		)

	def rmsProp(self, loss, params, rho, epsilon, learningRate):
		grads = theano.grad(cost=loss, wrt=params)

		updates = []
		for param, grad in zip(params, grads):
			value = param.get_value(borrow=True)
			acc = theano.shared(numpy.zeros(value.shape, dtype=value.dtype), broadcastable=param.broadcastable)
			accNew = rho * acc + (1 - rho) * grad ** 2
			updates.append((acc, accNew))
			updates.append((param, param - (learningRate * grad / T.sqrt(accNew + epsilon))))
		return updates

	def validate(self):
		errTotal = 0.0
		costTotal = 0.0
		for i in range(10):
			err, cost = self.validate_model(i)
			errTotal += err
			costTotal += cost
		return errTotal / 10.0, costTotal / 10.0

	def test(self):
		err = 0.0
		for i in range(10):
			err += self.test_model(i)
		return err / 10.0
		
	def predict(self, img):
		img = img.reshape(1,1,28,28).astype('float32')
		prob = self.forward(img)[0].flatten(1)
		prediction = numpy.argmax(prob, axis=0)
		prob = prob[prediction]
		return prediction, prob
		
	def getParams(self):
		return [param.get_value() for param in self.params]
		
	def setParams(self, params):
		for i in range(len(params)):
			self.params[i].set_value(params[i])
		

def loadData():
	with gzip.open('mnist.pkl.gz', 'rb') as f:
		train_set, valid_set, test_set = cPickle.load(f)
	train_x, train_y = train_set
	valid_x, valid_y = valid_set
	test_x, test_y = test_set

	return train_x, train_y, valid_x, valid_y, test_x, test_y
	
def showExamples(net, n, test_set):
	plt.figure()
	plt.ion()
	for i in range(n):
		index = numpy.random.randint(0, 10000)
		img = test_set[index,:]
		prediction, probability = net.predict(img)
		plt.imshow(img.reshape((28,28)), interpolation='none', cmap='gray')
		plt.title("Le reseau predit que c'est le chiffre " + str(prediction) + ' avec ' + str(probability*100) +'% de confiance')
		plt.show()
		raw_input('Appuyer sur [enter] pour continuer')

def main():
	train_x, train_y, valid_x, valid_y, test_x, test_y = loadData()
	
	net = None
	if len(sys.argv) >= 2:
		filename = sys.argv[1]
		f = open(filename, 'r')
		data = cPickle.load(f)
		f.close()
		net = data['net']
	else:
		batchSize = 128
		batchUpdates = 7600
		graphUpdateFreq = 20 #Number of updates after which the graph is updated
		graphUpdates = batchUpdates // graphUpdateFreq
		
		net = Net(train_x, train_y, valid_x, valid_y, test_x, test_y, batchSize)
		
		validationLosses = []
		validationErr = []
		trainingLosses = []
		
		plt.ion()
		fig = plt.figure()
		lossPlot = fig.add_subplot(211)
		lossPlot.axis([0,graphUpdates,-10,0.0])
		trainingLossLine, = lossPlot.plot([], [], label='Cout apprentissage')
		validationLossLine, = lossPlot.plot([], [], label='Cout validation')
		legend = lossPlot.legend(loc='lower left', shadow=True)
		plt.title("Fonction de cout sur l'ensemble de test et de validation en espace logarithmique")
		plt.xlabel('Nombre de batch updates (x20)')
		plt.ylabel('log(Perte)')
		plt.show()
		
		errPlot = fig.add_subplot(212)
		errPlot.axis([0, graphUpdates, 0, 0.3])
		errLine, = errPlot.plot([], [], label="Erreur sur l'ensemble de validation")
		legend = errPlot.legend(loc='upper right', shadow=True)
		plt.title("Erreur sur l'ensemble de validation")
		plt.xlabel('Nombre de batch updates (x20)')
		plt.ylabel('erreur')
		plt.show()
		
		fig.canvas.draw()
		
		for j in range(graphUpdates):
			loss = 0
			for i in range(graphUpdateFreq):
				loss += net.train_model(0.05)

			err, cost = net.validate()
			validationLosses.append(math.log(cost))
			trainingLosses.append(math.log(loss / graphUpdateFreq))
			validationErr.append(err)
			
			trainingLossLine.set_xdata(range(j+1))
			trainingLossLine.set_ydata(trainingLosses)
			validationLossLine.set_xdata(range(j+1))
			validationLossLine.set_ydata(validationLosses)
			errLine.set_xdata(range(j+1))
			errLine.set_ydata(validationErr)
			fig.canvas.draw()
			
		#Save the network
		data = {'net':net}
		f = open('network.pkl', 'w')
		cPickle.dump(data, f, -1)
		f.close()
		
	err = net.test()
	print 'ERREUR FINALE :', err
	
	showExamples(net, 20, test_x)

if __name__ == '__main__':
	main()
