import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1
input = open("input.txt", "r")

def mix(a, b):
    return a ^ b

def prune(x):
    return x % 16777216

def generate(x):
    tmp = x * 64
    x = prune(mix(x, tmp))
    tmp = x // 32
    x = prune(mix(x, tmp))
    tmp = x * 2048
    x = prune(mix(x, tmp))
    return x

ans1 = 0
changes = []
cnt = 0
for line in input:
    x = int(line)
    changes.append([])
    changes[cnt].append(x % 10)
    for _ in range(2000):
        x = generate(x)
        changes[cnt].append(abs(x) % 10)
    ans1 += x
    cnt += 1

print(ans1)

### Part 2
pos = {}
ans2 = 0
for i in range(cnt):
    vis = {}
    lst = changes[i]
    sum = 0
    for i in range(len(lst) - 1):
        sum = sum * 50 + lst[i + 1] - lst[i] + 30
        if i >= 4:
            sum -= (50 ** 4) * (30 + lst[i - 3] - lst[i - 4])
            if sum < 0:
                break
        if sum in vis:
            continue
        if sum not in pos:
            pos[sum] = 0
        vis[sum] = True
        pos[sum] += lst[i + 1]
        ans2 = max(ans2, pos[sum])

print(ans2)