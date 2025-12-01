### Part 1
input = open("input.txt", "r")
a, b = [], []

for lines in input:
    vec = lines.split()
    a.append(int(vec[0]))
    b.append(int(vec[1]))

a.sort()
b.sort()

ans = 0
for i in range(len(a)):
    ans += abs(a[i] - b[i])

print(ans)

### Part 2
dict = {}
for x in b:
    if x not in dict: dict[x] = 0
    dict[x] += 1

res = 0
for x in a:
    if x not in dict: dict[x] = 0
    res += x * dict[x]

print(res)