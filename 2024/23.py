import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1
input = open("input.txt", "r")
name = {}
cnt = 0
edge = [[] for _ in range(100000)]
st = []

for line in input:
    ed = line.strip().split('-')
    u, v = ed
    if u not in name:
        name[u] = cnt
        st.append(u)
        cnt += 1
    if v not in name:
        name[v] = cnt
        st.append(v)
        cnt += 1
    u, v = int(name[u]), int(name[v])
    edge[u].append(v)
    edge[v].append(u)

def startwitht(i):
    if st[i][0] == 't':
        return True
    return False

for i in range(cnt):
    edge[i].sort()

ans1 = 0
for i in range(cnt):
    for a in range(len(edge[i])):
        u = edge[i][a]
        if u <= i:
            continue
        for b in range(a + 1, len(edge[i])):
            v = edge[i][b]
            if v <= u:
                continue
            if v not in edge[u]:
                continue
            if startwitht(u) or startwitht(v) or startwitht(i):
                ans1 += 1

print(ans1)


### Part 2
ans2 = 0
res = []

def check(nodes, ans2):
    if len(nodes) <= ans2:
        return False
    for i in nodes:
        for j in nodes:
            if i == j:
                continue
            if j not in edge[i]:
                return False
    return True

for i in range(cnt):
    for mask in range(2 ** len(edge[i])):
        cur = [i]
        for k in range(len(edge[i])):
            if mask & (2 ** k) > 0:
                cur.append(edge[i][k])
        if check(cur, ans2):
            ans2 = len(cur)
            res = cur

res = [st[i] for i in res]
print(','.join(sorted(res)))
