import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import json # http://docs.python.org/3/library/json.html#json.loads
import math
import pdb

# NOTE:
#   - load json file
#   - np.vectorize for double 2d arrays
#   - draw contour
#     - matplotlib.pyplot
#     - http://matplotlib.org/examples/pylab_examples/contour_demo.html
#   - contour animation
#     - ax.cla(): http://stackoverflow.com/questions/14124903/animation-with-contours-matplotlib

# plt.xlim(-1, 1)
# plt.ylim(-1, 1)
plt.xlim(-2, 2)
plt.ylim(-2, 2)

## draw points ##
point_sets = json.loads(open("resources/Ml/BackPropagation/points1.json").read())
def draw_points():
    plt.plot(list(map(lambda x: x[0], point_sets[0])),
             list(map(lambda x: x[1], point_sets[0])),
             'b+',
             list(map(lambda x: x[0], point_sets[1])),
             list(map(lambda x: x[1], point_sets[1])),
             'ro')

## draw contour ##

# prepare contour region
xs = np.linspace(-1, 1, 100)
ys = np.linspace(-1, 1, 100)
X, Y = np.meshgrid(xs, ys)

### version 1: define contour function ###

def f(x):
    return 1 / (1 + math.exp(-x))

def h(w0_w1):
    w0 = np.array(w0_w1[0])
    w1 = np.array(w0_w1[1])
    vf = np.vectorize(f)
    def foo(x, y):
        inp = np.array([[x], [y], [1]])
        return f(w1.dot(vf(w0.dot(inp))))
    return foo

lines = json.loads(open("resources/Ml/BackPropagation/lines1.json").read())

fig = plt.figure()
ax = fig.gca()
def update_contour(num):
    Z = np.vectorize(h(lines[num]))(X, Y)
    ax.cla()
    draw_points()
    plt.contour(X, Y, Z, levels=[0.40, 0.5, 0.6])

plot_ani = animation.FuncAnimation(fig, update_contour, len(lines),
                                   interval=10, blit=False)

plot_ani.save("resources/Ml/BackPropagation/plot1.mp4")

### version 2: define contour function ###

# def f(x):
#     return 1 / (1 + math.exp(-x))

# def h(w):
#     return (lambda x, y: f(w[0] * x + w[1] * y + w[2]))

# lines = json.loads(open("resources/Ml/BackPropagation/lines_dumb1.json").read())

# fig = plt.figure()
# ax = fig.gca()
# def update_contour(num):
#     Z = np.vectorize(h(lines[num]))(X, Y)
#     ax.cla()
#     draw_points()
#     plt.contour(X, Y, Z, levels=[0.45, 0.5, 0.55])

# plot_ani = animation.FuncAnimation(fig, update_contour, len(lines),
#                                    interval=50, blit=False)

# plot_ani.save("resources/Ml/BackPropagation/plot.mp4")

plt.show()
