import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1 and 2
input = open("input.txt", "r")
dict = {}
conv = {'w' : 1, 'u' : 2, 'b' : 3, 'r' : 4, 'g' : 5}

def hash(st):
    sum = 0
    for c in st:
        sum = sum * 6 + conv[c]
    return sum

flag = True
ans1 = 0
ans2 = 0
mxlen = 0

def check(st):
    n = len(st)
    dp = [0 for _ in range(n + 2)]
    dp[0] = 1
    for i in range(n):
        if dp[i] == 0:
            continue
        hash = 0
        for j in range(mxlen):
            if i + j >= n:
                break
            hash = hash * 6 + conv[st[i + j]]
            if hash in dict:
                dp[i + j + 1] += dp[i]
    return dp[n]

for line in input:
    line = line.strip()
    if len(line) == 0:
        flag = False
        continue

    if flag:
        lst = line.split(', ')
        for pattern in lst:
            dict[hash(pattern)] = True
            mxlen = max(mxlen, len(pattern))
        continue
    
    way = check(line)
    ans2 += way
    if way > 0:
        ans1 += 1

print(ans1)
print(ans2)
