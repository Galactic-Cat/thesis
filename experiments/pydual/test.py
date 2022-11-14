from expression import Addition, Variable

x = Variable()
y = Variable()
t = Addition(Addition(x, y), x)

print(t.tree())