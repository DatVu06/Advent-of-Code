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
inf = int(1e9)

cost = [[[inf for _ in range(2)] for _ in range(m)] for _ in range(n)]
back = [[inf for _ in range(m)] for _ in range(n)]

def inrange(cx, cy):
    if cx < 0 or cy < 0 or cx >= n or cy >= m:
        return False
    return True

queue = deque()
queue.append((goalx, goaly))
back[goalx][goaly] = 0
while queue:
    ax, ay = queue.popleft()
    for dir in range(4):
        cx, cy = ax + dx[dir], ay + dy[dir]
        if inrange(cx, cy) == False or grid[cx][cy] == '#' or back[cx][cy] != inf:
            continue
        back[cx][cy] = back[ax][ay] + 1
        queue.append((cx, cy))

ans1 = 0
dist = back[x][y] - 100
queue.append((x, y, 0))
cost[x][y][0] = 0
while queue:
    ax, ay, cheat = queue.popleft()
    for dir in range(4):
        cx, cy = ax + dx[dir], ay + dy[dir]
        if inrange(cx, cy) == False:
            continue
        if cheat == 0 and grid[cx][cy] == '#' and cost[cx][cy][1] == inf:
            cost[cx][cy][1] = cost[ax][ay][cheat] + 1
            queue.append((cx, cy, 1))
            continue
        elif cheat == 1:
            if back[cx][cy] + 1 + cost[ax][ay][1] <= dist:
                ans1 += 1
            continue
        elif grid[cx][cy] != '#' and cost[cx][cy][cheat] == inf:
            cost[cx][cy][cheat] = cost[ax][ay][cheat] + 1
            queue.append((cx, cy, cheat))

print(ans1)


### Part 2
ans2 = 0

def check(sx, sy, cx, cy):
    if inrange(cx, cy) == False or grid[cx][cy] == '#':
        return False
    if cost[sx][sy][0] + abs(sx - cx) + abs(sy - cy) + back[cx][cy] <= dist:
        return True
    return False

for i in range(n):
    for j in range(m):
        if grid[i][j] == '#':
            continue
        for k in range(2, 21):
            for dx in range(k + 1):
                dy = k - abs(dx)
                cx, cy = i + dx, j + dy
                ncx, ncy = i - dx, j - dy
                if check(i, j, cx, cy) == True:
                    ans2 += 1
                if dx > 0 and check(i, j, ncx, cy) == True:
                    ans2 += 1
                if dy > 0 and check(i, j, cx, ncy) == True:
                    ans2 += 1
                if dx > 0 and dy > 0 and check(i, j, ncx, ncy) == True:
                    ans2 += 1

print(ans2)

