import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1
grid = []
input = open("input.txt", "r")

for line in input:
    line = line.strip()
    grid.append(list(line))

n, m = len(grid), len(grid[0])
x, y = 0, 0
goalx, goaly = 0, 0
for i in range(n):
    for j in range(m):
        if grid[i][j] == 'S':
            x, y = i, j
        if grid[i][j] == 'E':
            goalx, goaly = i, j

savex, savey = x, y
savegrid = [row[:] for row in grid]

dx = [0, +1, 0, -1]
dy = [+1, 0, -1, 0]

def calc(x, y, flag):
    cost = [[[int(1e9) for _ in range(4)] for _ in range(m)] for _ in range(n)]
    pq = PriorityQueue()
    if flag == False:
        cost[x][y][0] = 0
        pq.put((0, x, y, 0))
    else:
        for i in range(4):
            cost[x][y][i] = 0
            pq.put((0, x, y, i))

    while not pq.empty():
        t, ax, ay, dir = pq.get()
        if cost[ax][ay][dir] != t:
            continue
        if cost[ax][ay][(dir + 1) % 4] > t + 1000:
            cost[ax][ay][(dir + 1) % 4] = t + 1000
            pq.put((t + 1000, ax, ay, (dir + 1) % 4))
        if cost[ax][ay][(dir - 1) % 4] > t + 1000:
            cost[ax][ay][(dir - 1) % 4] = t + 1000
            pq.put((t + 1000, ax, ay, (dir - 1) % 4))

        cx, cy = ax + dx[dir], ay + dy[dir]
        if cx < 0 or cy < 0 or cx >= n or cy >= m or grid[cx][cy] == '#' or cost[cx][cy][dir] <= t + 1:
            continue
        cost[cx][cy][dir] = t + 1
        pq.put((t + 1, cx, cy, dir))
    return cost

cost = calc(x, y, False)
ans1 = min(cost[goalx][goaly])
print(ans1)


### Part 2
backCost = calc(goalx, goaly, True)
ans2 = 0
for i in range(n):
    for j in range(m):
        if grid[i][j] == '#':
            continue
        mincost = int(1e9)
        for d in range(4):
            mincost = min(mincost, cost[i][j][d] + backCost[i][j][(d + 2) % 4])
        if mincost == ans1:
            ans2 += 1
print(ans2)