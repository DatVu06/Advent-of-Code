import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1
grid = []
input = open("input.txt", "r")

for line in input:
    line = line.strip()
    grid.append(list(map(int, line)))

n, m = len(grid), len(grid[0])
ans1 = 0
dx = [+1, -1, 0, 0]
dy = [0, 0, +1, -1]

def find(x, y):
    queue = deque()
    queue.append((x, y))
    vis = [[False for _ in range(m)] for _ in range(n)]
    vis[x][y] = True
    res = 0
    while queue:
        ax, ay = queue.popleft()
        if grid[ax][ay] == 9:
            res += 1
        for dir in range(4):
            cx, cy = ax + dx[dir], ay + dy[dir]
            if cx < 0 or cy < 0 or cx >= n or cy >= m or vis[cx][cy] or grid[cx][cy] != grid[ax][ay] + 1:
                continue
            vis[cx][cy] = True
            queue.append((cx, cy))
    return res

for i in range(n):
    for j in range(m):
        if grid[i][j] != 0:
            continue
        ans1 += find(i, j)

print(ans1)


### Part 2
ans2 = 0

def get(x, y):
    queue = deque()
    queue.append((x, y))
    vis = [[0 for _ in range(m)] for _ in range(n)]
    vis[x][y] = 1
    res = 0
    while queue:
        ax, ay = queue.popleft()
        if grid[ax][ay] == 9:
            res += vis[ax][ay]
        for dir in range(4):
            cx, cy = ax + dx[dir], ay + dy[dir]
            if cx < 0 or cy < 0 or cx >= n or cy >= m or grid[cx][cy] != grid[ax][ay] + 1:
                continue
            vis[cx][cy] += vis[ax][ay]
            if vis[cx][cy] == vis[ax][ay]:
                queue.append((cx, cy))
    return res

for i in range(n):
    for j in range(m):
        if grid[i][j] != 0:
            continue
        ans2 += get(i, j)

print(ans2)