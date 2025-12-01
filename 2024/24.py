import re
import math
from collections import deque
from queue import PriorityQueue

### Part 1
input = open("input.txt", "r")
flag = False
var, ingate = {}, {}
gates, zvar, vis = [], [], []
cnt = 0
queue = deque()

def newVar(st):
    if st not in var:
        var[st] = -1
        ingate[st] = []
        if st[0] == 'z':
            zvar.append(st)

def cal(a, op, b):
    if op == 0:
        return var[a] & var[b]
    elif op == 1:
        return var[a] | var[b]
    return var[a] ^ var[b]

for line in input:
    line = line.strip()
    if len(line) == 0:
        flag = True
        continue
    if not flag:
        line = line.split(': ')
        newVar(line[0])
        var[line[0]] = int(line[1])
    else:
        line = line.split(' ')
        newVar(line[0])
        newVar(line[2])
        newVar(line[4])
        if line[1] == "AND":
            op = 0
        elif line[1] == "OR":
            op = 1
        else: op = 2
        ingate[line[0]].append(cnt)
        ingate[line[2]].append(cnt)
        gates.append((line[0], op, line[2], line[4]))
        vis.append(False)
        if var[line[0]] != -1 and var[line[2]] != -1:
            queue.append(cnt)
            var[line[4]] = cal(line[0], op, line[2])
            vis[cnt] = True
        cnt += 1

while queue:
    id = queue.popleft()
    a, op, b, c = gates[id]
    for x in ingate[c]:
        if vis[x] or var[gates[x][0]] == -1 or var[gates[x][2]] == -1:
            continue
        vis[x] = True
        queue.append(x)
        var[gates[x][3]] = cal(gates[x][0], gates[x][1], gates[x][2])

zvar.sort()
zvar.reverse()
ans1 = 0
for z in zvar:
    ans1 *= 2
    ans1 += var[z]
print(ans1)


### Part 2
ans2 = []

def findByOut(x):
    for i in range(cnt):
        if gates[i][3] == x:
            return i

def findGates(a, op, b):
    for i in range(cnt):
        if a == gates[i][0] and op == gates[i][1] and (b == gates[i][2] or b == ""):
            return i, gates[i][3]
        if a == gates[i][2] and op == gates[i][1] and (b == gates[i][0] or b == ""):
            return i, gates[i][3]
    return -1, ""
        
def swap(a, b):
    print(a, b)
    ans2.append(a)
    ans2.append(b)
    for i in range(cnt):
        if gates[i][3] == a:
            gates[i] = (gates[i][0], gates[i][1], gates[i][2], b)
        elif gates[i][3] == b:
            gates[i] = (gates[i][0], gates[i][1], gates[i][2], a)

save = ""

for i in range(45):
    num = "0" if i < 10 else ""
    num += str(i)
    x, y, z = "x" + num, "y" + num, "z" + num
    if i == 0:
        id, out = findGates(x, 2, y)
        if out != z:
            swap(out, z)
        id, out = findGates(x, 0, y)
        save = out
        continue

    id1, out1 = findGates(x, 2, y)
    id2, out2 = findGates(x, 0, y)

    id3, current = findGates(save, 2, out1)
    if id3 == -1:
        zgate = findByOut(z)
        if save == gates[zgate][0] or save == gates[zgate][2]:
            rem = gates[zgate][2] if save == gates[zgate][0] else gates[zgate][0]
            swap(out1, rem)
            if rem == out2: out1, out2 = out2, out1
            else: out1 = rem
        elif out1 == gates[zgate][0] or out1 == gates[zgate][2]:
            rem = gates[zgate][2] if out1 == gates[zgate][0] else gates[zgate][0]
            swap(save, rem)
            if rem == out2: rem, out2 = out2, rem
            else: save = rem
        else:
            print("sus")
    elif current != z:
        swap(current, z)
        if out2 == z: current, out2 = z, current
        else: current = z

    id3, current = findGates(save, 0, out1)
    id4, rem = findGates(current, 1, out2)

    if id4 == -1:
        try1, rem1 = findGates(current, 1, "")
        if try1 != -1:
            swapped = gates[try1][2] if current == gates[try1][0] else gates[try1][0]
            swap(out2, swapped)
            out2 = swapped
            save = rem1
        else:
            try1, rem1 = findGates(out2, 1, "")
            swapped = gates[try1][2] if out2 == gates[try1][0] else gates[try1][0]
            swap(current, swapped)
            current = swapped
            save = rem1
    else:
        save = rem

print(','.join(sorted(ans2)))