import random
import sys

values = int(sys.argv[1])
seed = int(sys.argv[2])
random.seed(seed)

min_value = -10_000
max_value = 20_000

for _ in range(values):
    print(random.randint(min_value, max_value))
