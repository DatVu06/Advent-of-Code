from collections import deque
import re

### Part 1
grid = []
input = open("input.txt", "r")

number_pattern = r'[-+]?\d+'
numbers = list(map(int, re.findall(number_pattern, input.read())))

a, b, c = numbers[0], numbers[1], numbers[2]
program = numbers[3:]

def exec(program, a, b, c):
    sz = len(program) // 2

    def lit(x):
        return x

    def combo(x):
        if x <= 3: return x
        elif x == 4: return a
        elif x == 5: return b
        elif x == 6: return c
        print("????????????")
        return c

    ans = []
    ptr = 0
    while ptr < sz * 2:
        op, val = program[ptr], program[ptr + 1]
        if op == 0:
            val = 2 ** combo(val)
            a = a // val
        elif op == 1:
            b = b ^ lit(val)
        elif op == 2:
            b = combo(val) % 8
        elif op == 3:
            if a == 0:
                ptr += 2
                continue
            ptr = lit(val)
            continue
        elif op == 4:
            b = b ^ c
        elif op == 5:
            ans.append(combo(val) % 8)
        elif op == 6:
            val = 2 ** combo(val)
            b = a // val
        elif op == 7:
            val = 2 ** combo(val)
            c = a // val
        ptr += 2
    return ans

ans1 = exec(program, a, b, c)
print(','.join(map(str, ans1)))
### Part 2
### program in input
# b = a % 8
# b = b ^ 2
# c = a // 2^b
# a = a // 8
# b = b ^ 7
# b = b ^ c
# out(b % 8)
# repeat

occur = {}
match = {}
for ca in range((1 << 10)):
    cb = ca % 8
    cb = cb ^ 2
    cc = ca // (2 ** cb)
    cb = cb ^ 7
    cb = cb ^ cc
    cb = cb % 8
    if cb not in occur:
        occur[cb] = []

    orgc, orgb = ca // 8, ca % 8
    occur[cb].append((orgc, orgb))

    if (orgc, cb) not in match:
        match[(orgc, cb)] = []
    match[(orgc, cb)].append(orgb)

for val in occur:
    occur[val] = sorted(occur[val])

for val in match:
    match[val] = sorted(match[val])

def update(l, r, bits, num):
    for i in range(r, l - 1, -1):
        bits[i] = num % 2
        num = num // 2

rev = program[::-1]

def backtrack(pos, next, bits):
    if pos == 48:
        return True
    
    save = bits[::]
    num = pos // 3
    target = rev[num]

    if next == -1:
        for val in occur[target]:
            f7, l3 = val[0], val[1]
            nxtval = (f7 % 16) * 8 + l3
            update(0, 9, bits, f7 * 8 + l3)
            if backtrack(pos + 3, nxtval, bits) == True:
                return True
            for i in range(len(bits)):
                bits[i] = save[i]
    else:
        if (next, target) not in match:
            return False
        for val in match[(next, target)]:
            nxtval = (next % 16) * 8 + val
            update(pos + 7, pos + 9, bits, val)
            if backtrack(pos + 3, nxtval, bits) == True:
                return True
            for i in range(len(bits)):
                bits[i] = save[i]
    return False


bits = [-1 for _ in range(55)]
valid = backtrack(0, -1, bits)
if valid == False:
    exit()
ans2 = 0
for x in bits:
    ans2 = ans2 * 2
    ans2 += x
print(ans2)
a = ans2
print(exec(program, a, b, c))
