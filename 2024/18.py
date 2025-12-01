import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1
input = open("input.txt", "r")
lst = []

for line in input:
    coord = line.strip().split(',')
    lst.append(coord)

n = 70
corrupt = 1024
grid = [['.' for _ in range(n + 1)] for _ in range(n + 1)]
for i in range(corrupt):
    x, y = lst[i]
    grid[int(x)][int(y)] = '#'

ans1 = 0
dx = [+1, -1, 0, 0]
dy = [0, 0, +1, -1]
inf = 1e9

def find():
    x, y = 0, 0
    goalx, goaly = n, n

    queue = deque()
    queue.append((x, y))

    dist = [[inf for _ in range(n + 1)] for _ in range(n + 1)]
    dist[x][y] = 0
    while queue:
        ax, ay = queue.popleft()
        for dir in range(4):
            cx, cy = ax + dx[dir], ay + dy[dir]
            if cx < 0 or cy < 0 or cx > n or cy > n or grid[cx][cy] == '#' or dist[cx][cy] < inf:
                continue
            dist[cx][cy] = dist[ax][ay] + 1
            queue.append((cx, cy))
    return dist

dist = find()
ans1 = dist[n][n]
print(ans1)


### Part 2
grid = [['.' for _ in range(n + 1)] for _ in range(n + 1)]
for i in range(len(lst)):
    x, y = lst[i]
    grid[int(x)][int(y)] = '#'
    dist = find()
    if dist[n][n] == inf:
        print(x + ',' + y)
        break
