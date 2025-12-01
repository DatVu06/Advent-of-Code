### Part 1
grid = []
input = open("input.txt", "r")

for line in input:
    grid.append(list(line.strip()))

dx = [-1, 0, +1, 0]
dy = [0, +1, 0, -1]
n, m = len(grid), len(grid[0])
vis = [[False] * m for _ in range(n)]
ans1 = 0
for i in range(n):
    for j in range(m):
        if grid[i][j] == '^':
            cur = (i, j)
            vis[i][j] = True
            ans1 += 1
            break
    if ans1:
        break

dir = 0
start = cur
while True:
    next = (cur[0] + dx[dir], cur[1] + dy[dir])
    if next[0] < 0 or next[0] >= n or next[1] < 0 or next[1] >= m:
        break
    if grid[next[0]][next[1]] == '#':
        dir = (dir + 1) % 4
        continue
    if vis[next[0]][next[1]] == False:
        vis[next[0]][next[1]] = True
        ans1 += 1
    cur = next

print(ans1)


### Part 2
def check():
    cur = start
    record = {}
    dir = 0
    record[(cur, dir)] = True
    while True:
        next = (cur[0] + dx[dir], cur[1] + dy[dir])
        if next[0] < 0 or next[0] >= n or next[1] < 0 or next[1] >= m:
            break
        if (next, dir) in record:
            return False
        record[(next, dir)] = True
        if grid[next[0]][next[1]] == '#':
            dir = (dir + 1) % 4
            continue
        cur = next
    
    return True

ans2 = 0
for i in range(n):
    for j in range(m):
        if grid[i][j] == '.':
            grid[i][j] = '#'
            if check() == False:
                ans2 += 1
            grid[i][j] = '.'

print(ans2)