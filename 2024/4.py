### Part 1
input = open("input.txt", "r")
grid = [line.strip() for line in input]
px = [-1, -1, -1, 0, 0, +1, +1, +1]
py = [-1, 0, +1, -1, +1, -1, 0, +1]
xmas = "XMAS"

n, m = len(grid), len(grid[0])
def check(x, y, dir):
    for i in range(4):
        cx, cy = x + px[dir] * i, y + py[dir] * i
        if (min(cx, cy) < 0 or cx >= n or cy >= m or grid[cx][cy] != xmas[i]):
            return False
    return True

ans1 = 0
for i in range(n):
    for j in range(m):
        for dir in range(8):
            if check(i, j, dir):
                ans1 += 1
print(ans1)


### Part 2
mas = "MAS"
sam = "SAM"

def check2(x, y, dx, dy, mas):
    for i in range(3):
        cx, cy = x + dx * i, y + dy * i
        if (min(cx, cy) < 0 or cx >= n or cy >= m or grid[cx][cy] != mas[i]):
            return False
    return True

ans2 = 0
for i in range(n - 2):
    for j in range(m - 2):
        if (check2(i, j, 1, 1, mas) or check2(i, j, 1, 1, sam)) and (check2(i, j + 2, 1, -1, mas) or check2(i, j + 2, 1, -1, sam)):
            ans2 += 1
print(ans2)