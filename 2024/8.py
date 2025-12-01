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
anti = [[False for _ in range(m)] for _ in range(n)]
ans1 = 0
pos = {}
for i in range(n):
    for j in range(m):
        if grid[i][j] != '.':
            if grid[i][j] not in pos:
                pos[grid[i][j]] = []
            pos[grid[i][j]].append((i, j))

def inrange(cx, cy):
    if cx < 0 or cy < 0 or cx >= n or cy >= m:
        return False
    return True

for _, val in pos.items():
    for i in range(len(val)):
        ax, ay = val[i]
        for j in range(i):
            bx, by = val[j]
            cx, cy = 2 * ax - bx, 2 * ay - by
            if inrange(cx, cy) and anti[cx][cy] == False:
                anti[cx][cy] = True
                ans1 += 1
            cx, cy = 2 * bx - ax, 2 * by - ay
            if inrange(cx, cy) and anti[cx][cy] == False:
                anti[cx][cy] = True
                ans1 += 1

print(ans1)


### Part 2
anti = [[False for _ in range(m)] for _ in range(n)]
ans2 = 0

for _, val in pos.items():
    for i in range(len(val)):
        ax, ay = val[i]
        if not anti[ax][ay]:
            ans2 += 1
        anti[ax][ay] = True
        
        for j in range(i):
            bx, by = val[j]
            cx, cy = 2 * ax - bx, 2 * ay - by
            while inrange(cx, cy):
                if not anti[cx][cy]:
                    ans2 += 1
                anti[cx][cy] = True
                cx += ax - bx
                cy += ay - by
            
            cx, cy = 2 * bx - ax, 2 * by - ay
            while inrange(cx, cy):
                if not anti[cx][cy]:
                    ans2 += 1
                anti[cx][cy] = True
                cx += bx - ax
                cy += by - ay

print(ans2)