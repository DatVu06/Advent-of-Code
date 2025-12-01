### Part 1
input = open("input.txt", "r")
ans = 0

def check(vec):
    increasing = True
    safe = True
    if vec[0] > vec[1]:
        increasing = False
    for i in range(len(vec) - 1):
        if increasing and vec[i] >= vec[i + 1]:
            safe = False
            break
        if not increasing and vec[i] <= vec[i + 1]:
            safe = False
            break
        diff = abs(vec[i] - vec[i + 1])
        if diff < 1 or diff > 3:
            safe = False
            break
    return safe
    
inp = []
for line in input:
    vec = list(map(int, line.split()))
    inp.append(vec)
    if check(vec):
        ans += 1

print(ans)


### Part 2
res = 0
for vec in inp:
    if check(vec):
        res += 1
        continue
    for i in range(len(vec)):
        if check(vec[:i] + vec[i + 1:]):
            res += 1
            break

print(res)