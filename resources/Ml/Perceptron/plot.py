import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

xs = np.linspace(-1, 1, 1000)
plt.xlim(-1, 1)
plt.ylim(-1, 1)

c0, c1 = open("resources/Ml/Perceptron/points.dat").read().strip().split("\n\n")
ls = open("resources/Ml/Perceptron/lines.dat").read().strip().split("\n")

def parse_points(lines):
    ls = lines.strip().split("\n")
    xys = list(map(lambda l: list(map(lambda s: float(s), l.split(" "))), ls))
    return (list(map(lambda xy: xy[0], xys)), list(map(lambda xy: xy[1], xys)))


def parse_line(line):
    a, b, c = line.strip().split(" ")
    return (float(a), float(b), float(c))

c0_xs, c0_ys = parse_points(c0)
c1_xs, c1_ys = parse_points(c1)

abcs = list(map(parse_line, ls))

fig = plt.figure()
plt.plot(c0_xs, c0_ys, 'b+', c1_xs, c1_ys, 'ro')

def yline(x, a, b, c):
    if b != 0:
        return - (a/b)*x - c/b
    else:
        raise BaseException

def update_line(num, data, line):
    a, b, c = data[num]
    line.set_data(xs, np.vectorize(lambda x: yline(x, a, b, c))(xs))
    return line,

l, = plt.plot([], [])
plot_ani = animation.FuncAnimation(fig, update_line, len(abcs), fargs=(abcs, l),
                                   interval=100, blit=False)

plot_ani.save("resources/Ml/Perceptron/plot.mp4")
