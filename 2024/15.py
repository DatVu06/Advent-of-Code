import re
import math
from collections import deque

### Part 1
grid = []
input = open("input.txt", "r")
flag = False
seq = ""

for line in input:
    line = line.strip()
    if len(line) == 0:
        flag = True
        continue
    if flag == True:
        seq += line
    else:
        grid.append(list(line))

n, m = len(grid), len(grid[0])
x, y = 0, 0
for i in range(n):
    for j in range(m):
        if grid[i][j] == '@':
            x, y = i, j
            break

savex, savey = x, y
savegrid = [row[:] for row in grid]

dx = [+1, -1, 0, 0]
dy = [0, 0, +1, -1]

def todir(c):
    if c == 'v': return 0
    elif c == '^': return 1
    elif c == '>': return 2
    return 3

for move in seq:
    dir = todir(move)
    cnt = 0
    empty = True
    while True:
        cx, cy = x + dx[dir] * (cnt + 1), y + dy[dir] * (cnt + 1)
        if grid[cx][cy] == '.':
            break
        if grid[cx][cy] == '#':
            empty = False
            break
        cnt += 1
    if empty == False:
        continue
    for i in range(cnt):
        cx, cy = x + dx[dir] * (i + 2), y + dy[dir] * (i + 2)
        grid[cx][cy] = 'O'
    grid[x][y] = '.'
    x, y = x + dx[dir], y + dy[dir]
    grid[x][y] = '@'

ans1 = 0
for i in range(n):
    for j in range(m):
        if grid[i][j] == 'O':
            ans1 += 100 * i + j

print(ans1)


### Part2
x, y = savex, savey
grid = savegrid
board = []
for i in range(n):
    cur = ""
    for j in range(m):
        if grid[i][j] == '#': cur += "##"
        elif grid[i][j] == 'O': cur += "[]"
        elif grid[i][j] == '.': cur += ".."
        else: cur += "@."
    board.append(list(cur))

m *= 2
y *= 2
print(x, y)

for move in seq:
    vis = [[False] * m for _ in range(n)]
    dir = todir(move)
    cnt = 0
    queue, save = deque(), deque()
    empty = True
    queue.append((x, y))
    vis[x][y] = True
    
    while queue:
        ax, ay = queue.popleft()
        save.append((ax, ay, board[ax][ay]))
        cx, cy = ax + dx[dir], ay + dy[dir]
        if board[cx][cy] == '#':
            empty = False
            break
        if board[cx][cy] == '.' or vis[cx][cy]:
            continue
        queue.append((cx, cy))
        vis[cx][cy] = True
        if board[cx][cy] == '[':
            queue.append((cx, cy + 1))
            vis[cx][cy + 1] = True
        else:
            queue.append((cx, cy - 1))
            vis[cx][cy - 1] = True

    if empty == False:
        continue
    
    while save:
        ax, ay, sym = save.pop()
        cx, cy = ax + dx[dir], ay + dy[dir]
        board[ax][ay] = '.'
        board[cx][cy] = sym
    board[x][y] = '.'
    board[x][y] = '.'
    x, y = x + dx[dir], y + dy[dir]
    board[x][y] = '@'

ans2 = 0
for i in range(n):
    for j in range(m):
        if board[i][j] == '[':
            ans2 += 100 * i + j

print(ans2)