from collections import deque
import re

### Part 1
grid = []
input = open("input.txt", "r")

number_pattern = r'[-+]?\d+'
numbers = list(map(int, re.findall(number_pattern, input.read())))

robots = len(numbers) // 4
n, m = 101, 103
ans1 = 1
count = [[0, 0], [0, 0]]
for t in range(robots):
    x, y, vx, vy = (numbers[t * 4 + i] for i in range(4))
    if vx < 0:
        vx %= n
        vx = n + vx
    if vy < 0:
        vy %= m
        vy = m + vy
    x, y = (x + 100 * vx) % n, (y + 100 * vy) % m
    if x < n // 2: qx = 0
    elif x > n // 2: qx = 1
    else: continue
    if y < m // 2: qy = 0
    elif y > m // 2: qy = 1
    else: continue
    count[qx][qy] += 1

for i in range(2):
    for j in range(2):
        ans1 *= count[i][j]

print(ans1)

### Part2
output = open("output.txt", "w")
dx = [+1, -1, 0, 0]
dy = [0, 0, +1, -1]

def move(time):
    grid = [[' ' for _ in range(m)] for _ in range(n)]
    for t in range(robots):
        x, y, vx, vy = (numbers[t * 4 + i] for i in range(4))
        if vx < 0:
            vx %= n
            vx = n + vx
        if vy < 0:
            vy %= m
            vy = m + vy
        x, y = (x + time * vx) % n, (y + time * vy) % m
        grid[x][y] = '*'

    component = 0
    vis = [[False] * m for _ in range(n)]
    
    for i in range(n):
        for j in range(m):
            if vis[i][j] == True or grid[i][j] == ' ':
                continue
            component += 1
            queue = deque()
            queue.append((i, j))
            while queue:
                tx, ty = queue.popleft()
                for dir in range(4):
                    cx, cy = tx + dx[dir], ty + dy[dir]
                    if cx < 0 or cy < 0 or cx >= n or cy >= m or vis[cx][cy] == True or grid[cx][cy] != '*':
                        continue
                    vis[cx][cy] = True
                    queue.append((cx, cy))

    if component > 350:
        return
    print(component, time)
    output.write("Time " + str(time) + "\n")
    for i in range(n):
        output.write(''.join(grid[i]) + "\n")
    output.write("**" * 100 + "\n")

for i in range(10000):
    move(i)
output.close()
