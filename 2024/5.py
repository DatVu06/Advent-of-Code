from collections import deque

### Part 1
input = open("input.txt", "r")
rules = []
ans1 = 0
incorrect = []

def check(lst):
    dict = {}
    for i in range(len(lst)):
        dict[lst[i]] = i
    for rule in rules:
        a, b = rule
        if a not in dict or b not in dict:
            continue
        if dict[a] > dict[b]:
            return False
    return True

for line in input:
    line = line.strip()
    if len(line) == 0:
        continue
    if '|' in line:
        rule = line.split("|")
        rules.append((int(rule[0]), int(rule[1])))
    else:
        lst = list(map(int, line.split(",")))
        if check(lst):
            ans1 += lst[len(lst) // 2]
        else:
            incorrect.append(lst)

print(ans1)


### Part 2
def reorder(lst):
    adj = {}
    deg = {}
    for u in lst:
        deg[u] = 0
        adj[u] = []

    for rule in rules:
        u, v = rule
        if u not in deg or v not in deg:
            continue
        deg[v] += 1
        adj[u].append(v)

    queue = deque()
    pos = 0
    for u in lst:
        if deg[u] == 0:
            queue.append(u)
    while queue:
        u = queue.popleft()
        if pos == len(lst) // 2:
            return u
        pos += 1
        for v in adj[u]:
            deg[v] -= 1
            if deg[v] == 0:
                queue.append(v)
    return 0

ans2 = 0
for lst in incorrect:
    ans2 += reorder(lst)

print(ans2)
