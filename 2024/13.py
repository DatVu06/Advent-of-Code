import re
import math

### Part 1
grid = []
input = open("input.txt", "r")

number_pattern = r'[-+]?\d+'
numbers = list(map(int, re.findall(number_pattern, input.read())))

query = len(numbers) // 6
inf = int(1e18)
ans1 = 0
for t in range(query):
    ax, ay, bx, by, x, y = (numbers[t * 6 + i] for i in range(6))
    count = 0
    minToken = inf
    while True:
        if x < 0 or y < 0:
            break
        if x % bx == 0 and y % by == 0 and x / bx == y / by:
            minToken = min(minToken, 3 * count + x // bx)
        x -= ax
        y -= ay
        count += 1
    if minToken < inf:
        ans1 += minToken
print(ans1)


### Part 2
add = 10000000000000
ans2 = 0

for t in range(query):
    ax, ay, bx, by, x, y = (numbers[t * 6 + i] for i in range(6))
    x += add
    y += add
    # Hard cases
    if ax * by == ay * bx:
        continue
    resa = (y - x / bx * by) / (ay - ax / bx * by)
    resb = (x - ax * resa) / bx
    resa = int(resa)
    resb = int(resb)
    savea, saveb = resa, resb
    for i in range(-100, 100):
        for j in range(-100, 100):
            resa, resb = savea + i, saveb + j
            if resa >= 0 and resb >= 0 and ax * resa + bx * resb == x and ay * resa + by * resb == y:
                ans2 += 3 * resa + resb

print(ans2)