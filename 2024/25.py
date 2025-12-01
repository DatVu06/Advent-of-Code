import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1
input = open("input.txt", "r")
cur = []
lock, key = [], []

for line in input:
    line = line.strip()
    if len(line) == 0:
        if cur[0][0] == '#':
            lock.append([row[:] for row in cur])
        else:
            key.append([row[:] for row in cur[::-1]])
        cur = []
    else:
        cur.append(list(line))

if cur[0][0] == '#':
    lock.append([row[:] for row in cur])
else:
    key.append([row[:] for row in cur[::-1]])

nlock, nkey = [], []
cntL, cntK = 0, 0
for tmp in lock:
    nlock.append([])
    for j in range(len(tmp[0])):
        cnt = -1
        for i in range(len(tmp)):
            if tmp[i][j] == '.':
                break
            cnt += 1
        nlock[cntL].append(cnt)
    cntL += 1

for tmp in key:
    nkey.append([])
    for j in range(len(tmp[0])):
        cnt = -1
        for i in range(len(tmp)):
            if tmp[i][j] == '.':
                break
            cnt += 1
        nkey[cntK].append(cnt)
    cntK+= 1

ans1 = 0

def isCompatible(l, k):
    for i in range(5):
        if l[i] + k[i] >= 6:
            return False
    return True

for l in nlock:
    for k in nkey:
        if isCompatible(l, k):
            ans1 += 1

print(ans1)