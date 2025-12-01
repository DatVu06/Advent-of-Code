from collections import deque

### Part 1
grid = []
input = open("input.txt", "r")

for line in input:
    grid.append(list(line.strip()))

n, m = len(grid), len(grid[0])
vis = [[False] * m for _ in range(n)]

dx = [+1, -1, 0, 0]
dy = [0, 0, +1, -1]
ans1 = 0
for i in range(n):
    for j in range(m):
        if vis[i][j] == True:
            continue
        area = 0
        perimeter = 0
        queue = deque()
        queue.append((i, j))
        vis[i][j] = True
        while queue:
            x, y = queue.popleft()
            area += 1
            for dir in range(4):
                cx, cy = x + dx[dir], y + dy[dir]
                if cx < 0 or cy < 0 or cx >= n or cy >= m or grid[i][j] != grid[cx][cy]:
                    perimeter += 1
                    continue
                if vis[cx][cy] == False:
                    vis[cx][cy] = True
                    queue.append((cx, cy))
        ans1 += area * perimeter

print(ans1)


### Part 2
ans2 = 0
counter = 0
vis = [[-1] * m for _ in range(n)]
for i in range(n):
    for j in range(m):
        if vis[i][j] != -1:
            continue
        area = 0
        sides = 0
        counter += 1
        queue = deque()
        queue.append((i, j))
        vertical = {}
        horizontal = {}
        vis[i][j] = counter
        while queue:
            x, y = queue.popleft()
            area += 1
            for dir in range(4):
                cx, cy = x + dx[dir], y + dy[dir]
                if cx < 0 or cy < 0 or cx >= n or cy >= m or grid[i][j] != grid[cx][cy]:
                    sides += 1
                    if dir < 2: horizontal[(max(cx, x), y, dir)] = True
                    else: vertical[(x, max(cy, y), dir)] = True
                    continue
                if vis[cx][cy] == -1:
                    vis[cx][cy] = counter
                    queue.append((cx, cy))

        for x, y, dir in vertical:
            if (x - 1, y, dir) not in vertical:
                continue
            sides -= 1
        for x, y, dir in horizontal:
            if (x, y - 1, dir) not in horizontal:
                continue
            sides -= 1
        ans2 += sides * area

print(ans2)