import re
import math
import heapq
from collections import deque

### Part 1
input = open("input.txt", "r")

inf = 1e12
direct = [[(1, 2)], [(0, 0), (2, 2), (3, 3)], [(1, 0), (4, 3)], [(1, 1), (4, 2)], [(2, 1), (3, 0)]]
level1 = [[inf for _ in range(5)] for _ in range(5)]

for i in range(5):
    level1[i][i] = 0
    for x, _ in direct[i]:
        level1[i][x] = 1

for k in range(5):
    for i in range(5):
        for j in range(5):
            level1[i][j] = min(level1[i][j], level1[i][k] + level1[k][j])

level2 = {}
pr = []
for i in range(5):
    level2[(i, i, 4)] = 0
    heapq.heappush(pr, (0, i, i, 4))

while pr:
    val, u, v, cur = heapq.heappop(pr)
    if level2[(u, v, cur)] != val:
        continue
    for x, nxt in direct[v]:
        if (u, x, nxt) not in level2 or val + level1[cur][nxt] + 1 < level2[(u, x, nxt)]:
            level2[(u, x, nxt)] = val + level1[cur][nxt] + 1
            heapq.heappush(pr, (level2[(u, x, nxt)], u, x, nxt))
    if (u, v, 4) not in level2 or val + level1[cur][4] < level2[(u, v, 4)]:
        level2[(u, v, 4)] = val + level1[cur][4]
        heapq.heappush(pr, (level2[(u, v, 4)], u, v, 4))

level3 = {}
grid = ["789", "456", "123", "#0A"]
def decode(i, j):
    if grid[i][j] == '#': return -1
    elif grid[i][j] == 'A': return 10
    else: return int(grid[i][j])

adj = [[] for _ in range(11)]
dx = [0, +1, 0, -1]
dy = [-1, 0, +1, 0]
for i in range(4):
    for j in range(3):
        id = decode(i, j)
        if id == -1: continue
        for dir in range(4):
            cx, cy = i + dx[dir], j + dy[dir]
            if cx < 0 or cy < 0 or cx >= 4 or cy >= 3 or decode(cx, cy) == -1:
                continue
            adj[id].append((decode(cx, cy), dir))
    
pr = []
for i in range(11):
    level3[(i, i, 4)] = 0
    heapq.heappush(pr, (0, i, i, 4))

while pr:
    val, u, v, cur = heapq.heappop(pr)
    if level3[(u, v, cur)] != val:
        continue
    for x, nxt in adj[v]:
        if (u, x, nxt) not in level3 or val + level2[(cur, nxt, 4)] + 1 < level3[(u, x, nxt)]:
            level3[(u, x, nxt)] = val + level2[(cur, nxt, 4)]+ 1
            heapq.heappush(pr, (level3[(u, x, nxt)], u, x, nxt))
    if (u, v, 4) not in level3 or val + level2[(cur, 4, 4)] < level3[(u, v, 4)]:
        level3[(u, v, 4)] = val + level2[(cur, 4, 4)]
        heapq.heappush(pr, (level3[(u, v, 4)], u, v, 4))


def solve(code):
    cur = 10
    step = 0
    for c in code:
        if c == 'A': nxt = 10
        else: nxt = int(c)
        step += level3[(cur, nxt, 4)] + 1
        cur = nxt
    print(step)
    return step * int(code[:-1])

ans1 = 0
lst = []
for line in input:
    lst.append(line.strip())
    ans1 += solve(line.strip())

print(ans1)


### Part 2
def upLevel(level2):
    level3 = {}
    pr = []
    for i in range(5):
        level3[(i, i, 4)] = 0
        heapq.heappush(pr, (0, i, i, 4))

    while pr:
        val, u, v, cur = heapq.heappop(pr)
        if level3[(u, v, cur)] != val:
            continue
        for x, nxt in direct[v]:
            if (u, x, nxt) not in level3 or val + level2[(cur, nxt, 4)] + 1 < level3[(u, x, nxt)]:
                level3[(u, x, nxt)] = val + level2[(cur, nxt, 4)]+ 1
                heapq.heappush(pr, (level3[(u, x, nxt)], u, x, nxt))
        if (u, v, 4) not in level3 or val + level2[(cur, 4, 4)] < level3[(u, v, 4)]:
            level3[(u, v, 4)] = val + level2[(cur, 4, 4)]
            heapq.heappush(pr, (level3[(u, v, 4)], u, v, 4))
    return level3

for i in range(23):
    level2 = upLevel(level2)

level3 = {}
pr = []
for i in range(11):
    level3[(i, i, 4)] = 0
    heapq.heappush(pr, (0, i, i, 4))

while pr:
    val, u, v, cur = heapq.heappop(pr)
    if level3[(u, v, cur)] != val:
        continue
    for x, nxt in adj[v]:
        if (u, x, nxt) not in level3 or val + level2[(cur, nxt, 4)] + 1 < level3[(u, x, nxt)]:
            level3[(u, x, nxt)] = val + level2[(cur, nxt, 4)]+ 1
            heapq.heappush(pr, (level3[(u, x, nxt)], u, x, nxt))
    if (u, v, 4) not in level3 or val + level2[(cur, 4, 4)] < level3[(u, v, 4)]:
        level3[(u, v, 4)] = val + level2[(cur, 4, 4)]
        heapq.heappush(pr, (level3[(u, v, 4)], u, v, 4))

ans2 = 0
for line in lst:
    ans2 += solve(line)

print(ans2)